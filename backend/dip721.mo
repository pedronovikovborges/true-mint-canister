import Error "mo:base/Error";
import Hash "mo:base/Hash";
import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Nat32 "mo:base/Nat32";
import Option "mo:base/Option";
import Principal "mo:base/Principal";
import Array "mo:base/Array";
import Text "mo:base/Text";
import List "mo:base/List";
import Iter "mo:base/Iter";
import Char "mo:base/Char";
import Cycles "mo:base/ExperimentalCycles";
import T "types_dip721";
import Http "types_http";

actor class NFT721(_minter: Principal, _name: Text, _symbol: Text) {
    private stable var tokenCounter: Nat = 0;
    private stable var tokenURIEntries: [(T.TokenId, Text)] = [];
    private stable var ownerEntries: [(T.TokenId, Principal)] = [];
    private stable var balanceEntries: [(Principal, Nat)] = [];
    private stable var tokenApprovalEntries: [(T.TokenId, Principal)] = [];
    private stable var operatorApprovalEntries: [(Principal, [Principal])] = [];
    private stable var templateState: [(Text, HttpTemplate)] = [];

    private let tokenURIs: HashMap.HashMap<T.TokenId, Text> = HashMap.fromIter<T.TokenId, Text>(tokenURIEntries.vals(), 10, Nat.equal, Hash.hash);
    private let owners: HashMap.HashMap<T.TokenId, Principal> = HashMap.fromIter<T.TokenId, Principal>(ownerEntries.vals(), 10, Nat.equal, Hash.hash);
    private let balances: HashMap.HashMap<Principal, Nat> = HashMap.fromIter<Principal, Nat>(balanceEntries.vals(), 10, Principal.equal, Principal.hash);
    private let tokenApprovals: HashMap.HashMap<T.TokenId, Principal> = HashMap.fromIter<T.TokenId, Principal>(tokenApprovalEntries.vals(), 10, Nat.equal, Hash.hash);
    private let operatorApprovals: HashMap.HashMap<Principal, [Principal]> = HashMap.fromIter<Principal, [Principal]>(operatorApprovalEntries.vals(), 10, Principal.equal, Principal.hash);
    private let admins: HashMap.HashMap<Principal, Bool> = HashMap.HashMap<Principal, Bool>(0, Principal.equal, Principal.hash);
    private var templates: HashMap.HashMap<Text, HttpTemplate> = HashMap.fromIter(templateState.vals(), 0, Text.equal, Text.hash);

    private stable var totalNFTs = 0;
    private stable var assetCanisterURI = "_";
    private stable var uriPattern = "{{tokenid}}";
    private stable var mintAssetIds: [Nat] = [];

    type HttpTemplate = {
        template: Text;
        contentType: Text;
        pattern: Text;
    };

    public query func tokensOf(owner: Principal): async [T.TokenId] {
        let ledger = owners.entries();
        var result: [T.TokenId] = [];
        for ((tokenId, tokenOwner) in ledger) {
            if (tokenOwner == owner) {
                result := Array.append(result, [tokenId]);
            };
        };
        return result;
    };

    public query func tokens(): async [T.TokenId] {
        let ledger = owners.entries();
        var result: [T.TokenId] = [];
        for ((tokenId, _) in ledger) {
            result := Array.append(result, [tokenId]);
        };
        return result;
    };

    public shared func balanceOf(owner: Principal): async ?Nat {
        return balances.get(owner);
    };

    public shared func ownerOf(tokenId: T.TokenId): async ?Principal {
        return _ownerOf(tokenId);
    };

    public shared query func tokenURI(tokenId: T.TokenId): async ?Text {
        return _tokenURI(tokenId);
    };

    public shared query func name(): async Text {
        return _name;
    };

    public shared query func symbol(): async Text {
        return _symbol;
    };

    public shared query func total(): async Nat {
        return totalNFTs;
    };

    public shared query func mintRemaining(): async Nat {
        return totalNFTs - tokenCounter;
    };

    public shared func isApprovedForAll(owner: Principal, operator: Principal): async Bool {
        return _isApprovedForAll(owner, operator);
    };

    public shared(msg) func approve(to: Principal, tokenId: T.TokenId): async () {
        switch (_ownerOf(tokenId)) {
            case (?owner) {
                assert to != owner;
                assert msg.caller == owner or _isApprovedForAll(owner, msg.caller);
                _approve(to, tokenId);
            };
            case (null) {
                throw Error.reject("No owner for token");
            };
        }
    };

    public shared func getApproved(tokenId: Nat): async Principal {
        switch (_getApproved(tokenId)) {
            case (?approved) { return approved };
            case null { throw Error.reject("None approved") };
        }
    };

    public shared(msg) func setApprovalForAll(operator: Principal, isApproved: Bool): () {
        assert msg.caller != operator;

        switch (isApproved) {
            case true {
                switch (operatorApprovals.get(msg.caller)) {
                    case (?opList) {
                        var array = Array.filter<Principal>(opList, func(p) { p != operator });
                        array := Array.append<Principal>(array, [operator]);
                        operatorApprovals.put(msg.caller, array);
                    };
                    case null {
                        operatorApprovals.put(msg.caller, [operator]);
                    };
                };
            };
            case false {
                switch (operatorApprovals.get(msg.caller)) {
                    case (?opList) {
                        let array = Array.filter<Principal>(opList, func(p) { p != operator });
                        operatorApprovals.put(msg.caller, array);
                    };
                    case null {
                        operatorApprovals.put(msg.caller, []);
                    };
                };
            };
        };
    };

    public shared(msg) func transferFrom(from: Principal, to: Principal, tokenId: Nat): () {
        assert _isApprovedOrOwner(msg.caller, tokenId);
        _transfer(from, to, tokenId);
    };

    public shared(msg) func mint(quantity: Nat): async [Nat] {
        if ((tokenCounter + quantity) > totalNFTs) {
            return [0];
        };
        var i = quantity;
        var result: [Nat] = [];
        while (i > 0) {
            let assetIndex = mintAssetIds[tokenCounter];
            let uri: Text = _buildNFTUri(assetCanisterURI, assetIndex);
            _mint(msg.caller, assetIndex, uri);
            result := Array.append(result, [assetIndex]);
            tokenCounter += 1;
            i -= 1;
        };
        return result;
    };

    private func _ownerOf(tokenId: T.TokenId): ?Principal {
        return owners.get(tokenId);
    };

    private func _tokenURI(tokenId: T.TokenId): ?Text {
        return tokenURIs.get(tokenId);
    };

    private func _isApprovedForAll(owner: Principal, operator: Principal): Bool {
        switch (operatorApprovals.get(owner)) {
            case (?whiteList) {
                for (allowed in whiteList.vals()) {
                    if (allowed == operator) {
                        return true;
                    };
                };
            };
            case null { return false; };
        };
        return false;
    };

    private func _approve(to: Principal, tokenId: Nat): () {
        tokenApprovals.put(tokenId, to);
    };

    private func _removeApproval(tokenId: Nat): () {
        let _ = tokenApprovals.remove(tokenId);
    };

    private func _exists(tokenId: Nat): Bool {
        return Option.isSome(owners.get(tokenId));
    };

    private func _getApproved(tokenId: Nat): ?Principal {
        assert _exists(tokenId) == true;
        switch (tokenApprovals.get(tokenId)) {
            case (?approved) { return ?approved };
            case null { return null };
        }
    };

    private func _hasApprovedAndSame(tokenId: Nat, spender: Principal): Bool {
        switch (_getApproved(tokenId)) {
            case (?approved) {
                return approved == spender;
            };
            case null { return false };
        }
    };

    private func _isApprovedOrOwner(spender: Principal, tokenId: Nat): Bool {
        assert _exists(tokenId);
        let owner = Option.unwrap(_ownerOf(tokenId));
        return spender == owner or _hasApprovedAndSame(tokenId, spender) or _isApprovedForAll(owner, spender);
    };

    private func _transfer(from: Principal, to: Principal, tokenId: Nat): () {
        assert _exists(tokenId);
        assert Option.unwrap(_ownerOf(tokenId)) == from;

        _removeApproval(tokenId);
        _decrementBalance(from);
        _incrementBalance(to);
        owners.put(tokenId, to);
    };

    private func _incrementBalance(address: Principal) {
        switch (balances.get(address)) {
            case (?balance) {
                balances.put(address, balance + 1);
            };
            case null {
                balances.put(address, 1);
            };
        }
    };

    private func _decrementBalance(address: Principal) {
        switch (balances.get(address)) {
            case (?balance) {
                balances.put(address, balance - 1);
            };
            case null {
                balances.put(address, 0);
            };
        }
    };

    private func _mint(to: Principal, tokenId: Nat, uri: Text): () {
        assert not _exists(tokenId);

        _incrementBalance(to);
        owners.put(tokenId, to);
        tokenURIs.put(tokenId, uri);
    };

    private func _burn(tokenId: Nat) {
        let owner = Option.unwrap(_ownerOf(tokenId));

        _removeApproval(tokenId);
        _decrementBalance(owner);

        ignore owners.remove(tokenId);
    };

    system func preupgrade() {
        tokenURIEntries := Iter.toArray(tokenURIs.entries());
        ownerEntries := Iter.toArray(owners.entries());
        balanceEntries := Iter.toArray(balances.entries());
        tokenApprovalEntries := Iter.toArray(tokenApprovals.entries());
        operatorApprovalEntries := Iter.toArray(operatorApprovals.entries());
        templateState := Iter.toArray(templates.entries());
    };

    system func postupgrade() {
        tokenURIEntries := [];
        ownerEntries := [];
        balanceEntries := [];
        tokenApprovalEntries := [];
        operatorApprovalEntries := [];
        templateState := [];
    };

    public shared(msg) func initialize(total: Nat): async () {
        assert((msg.caller == _minter) or _isAdmin(msg.caller));
        totalNFTs := total;

        let assetArray = Array.tabulateVar<Nat>(totalNFTs, func(index): Nat {
            return index + 1;
        });

        var i = 0;
        while (i < totalNFTs) {
            _shuffle(assetArray, totalNFTs);
            i += 1;
        };

        mintAssetIds := Array.freeze(assetArray);
    };

    private func _shuffle(assetArray: [var Nat], total: Nat): () {
        var x: Nat = 64;
        var y: Nat = total - x;

        var xJump = 1;
        var yJump = 2;

        while (x < total and y > 0) {
            _swap(assetArray, x, y);
            x += xJump;
            if (yJump > y) {
                y := total - 64;
            };
            y -= yJump;

            xJump += 2;
            yJump += 1;
        };

        x := 32;
        y := total - x;

        xJump := 3;
        yJump := 5;
        while (x < total and y > 0) {
            _swap(assetArray, x, y);
            x += xJump;
            if (yJump > y) {
                y := total - 32;
            };
            y -= yJump;

            xJump += 1;
            yJump += 1;
        };

        x := 128;
        y := total - x;

        xJump := 1;
        yJump := 4;
        while (x < total and y > 0) {
            _swap(assetArray, x, y);
            x += xJump;
            if (yJump > y) {
                y := total - 128;
            };
            y -= yJump;

            xJump += 2;
            yJump += 1;
        };

        x := 0;
        y := total / 2;

        while (y < total) {
            _swap(assetArray, x, y);
            x += 1;
            y += 1;
        };
    };

    private func _swap(arr: [var Nat], i: Nat, j: Nat): () {
        let temp = arr[i];
        arr[i] := arr[j];
        arr[j] := temp;
    };

    private func _buildNFTUri(uri: Text, tokenId: Nat): Text {
        let result = Text.replace(assetCanisterURI, #text(uriPattern), Nat.toText(tokenId));
        return result;
    };

    public shared(msg) func setUriPattern(pattern: Text): async Bool {
        assert((msg.caller == _minter) or _isAdmin(msg.caller));
        uriPattern := pattern;
        assert(uriPattern == pattern);
        return true;
    };

    public shared(msg) func setAssetCanisterUri(uri: Text): async Bool {
        assert((msg.caller == _minter) or _isAdmin(msg.caller));
        assetCanisterURI := uri;
        assert(assetCanisterURI == uri);
        let tokensUri = tokenURIs.entries();
        for ((tokenId, _) in tokensUri) {
            let tokenUri = _buildNFTUri(assetCanisterURI, tokenId);
            tokenURIs.put(tokenId, tokenUri);
        };
        return true;
    };

    private func _isAdmin(principal: Principal): Bool {
        let value = admins.get(principal);
        switch (value) {
            case (?isAdmin) return isAdmin;
            case (null) return false;
        };
    };

    public shared(msg) func addAdmin(principal: Principal): async Bool {
        assert(msg.caller == _minter);
        admins.put(principal, true);
        return _isAdmin(principal);
    };

    public func acceptCycles(): async () {
        let available = Cycles.available();
        let accepted = Cycles.accept(available);
        assert (accepted == available);
    };

    public query func availableCycles(): async Nat {
        return Cycles.balance();
    };

    public shared(msg) func setTemplate(key: Text, template: HttpTemplate): async () {
        assert((msg.caller == _minter) or _isAdmin(msg.caller));
        templates.put(key, template);
    };

    public query(msg) func getTemplate(key: Text): async HttpTemplate {
        assert((msg.caller == _minter) or _isAdmin(msg.caller));
        return Option.unwrap(templates.get(key));
    };

    public query func httpRequest(request: Http.HttpRequest): async Http.HttpResponse {
        let token = Option.get(_getParam(request.url, "tokenid"), "");
        let tokenId: Nat = Nat32.toNat(_textToNat32(token));
        if (tokenId == 0 or tokenId > totalNFTs) {
            {
                status_code = 200;
                headers = [("content-type", "text/plain")];
                body = Text.encodeUtf8(
                    "Cycle Balance:   ~" # debug_show(Cycles.balance() / 1000000000000) # "T\n"
                );
                streaming_strategy = null;
            };
        } else {
            let templateType = Option.get(_getParam(request.url, "type"), "");
            let httpTemplate: HttpTemplate = Option.unwrap(templates.get(templateType));

            let tokenUri = tokenURIs.get(tokenId);
            switch (tokenUri) {
                case (?uri) {
                    let assetUrl = Text.replace(uri, #text("ic0.app"), "raw.ic0.app");
                    let templateData = Text.replace(httpTemplate.template, #text(httpTemplate.pattern), assetUrl);
                    {
                        status_code = 200;
                        headers = [("Content-Type", httpTemplate.contentType)];
                        body = Text.encodeUtf8(templateData);
                        streaming_strategy = null;
                    };
                };
                case (null) {
                    _buildNotFoundHttpResponseWithBodyText("Not mint");
                };
            };
        };
    };

    private func _buildNotFoundHttpResponseWithBodyText(err: Text): Http.HttpResponse {
        {
            status_code = 404;
            headers = [];
            body = Text.encodeUtf8(err);
            streaming_strategy = null;
        };
    };

    private func _textToNat32(t: Text): Nat32 {
        var reversed: [Nat32] = [];
        for (c in t.chars()) {
            assert(Char.isDigit(c));
            reversed := Array.append([Char.toNat32(c) - 48], reversed);
        };
        var total: Nat32 = 0;
        var place: Nat32 = 1;
        for (v in reversed.vals()) {
            total += (v * place);
            place := place * 10;
        };
        total;
    };

    private func _getParam(url: Text, param: Text): ?Text {
        var _s: Text = url;
        Iter.iterate<Text>(Text.split(_s, #text("/")), func(x, _i) {
            _s := x;
        });
        Iter.iterate<Text>(Text.split(_s, #text("?")), func(x, _i) {
            if (_i == 1) _s := x;
        });
        var t: ?Text = null;
        var found: Bool = false;
        Iter.iterate<Text>(Text.split(_s, #text("&")), func(x, _i) {
            if (found == false) {
                Iter.iterate<Text>(Text.split(x, #text("=")), func(y, _ii) {
                    if (_ii == 0) {
                        if (Text.equal(y, param)) found := true;
                    } else if (found == true) t := ?y;
                });
            };
        });
        return t;
    };
};

    private func _shuffle(assetArray: [var Nat], total: Nat): () {
        var x: Nat = 64;
        var y: Nat = total - x;

        var xJump = 1;
        var yJump = 2;

        while (x < total and y > 0) {
            _swap(assetArray, x, y);
            x += xJump;
            if (yJump > y) {
                y := total - 64;
            };
            y -= yJump;

            xJump += 2;
            yJump += 1;
        };

        x := 32;
        y := total - x;

        xJump := 3;
        yJump := 5;
        while (x < total and y > 0) {
            _swap(assetArray, x, y);
            x += xJump;
            if (yJump > y) {
                y := total - 32;
            };
            y -= yJump;

            xJump += 1;
            yJump += 1;
        };

        x := 128;
        y := total - x;

        xJump := 1;
        yJump := 4;
        while (x < total and y > 0) {
            _swap(assetArray, x, y);
            x += xJump;
            if (yJump > y) {
                y := total - 128;
            };
            y -= yJump;

            xJump += 2;
            yJump += 1;
        };

        x := 0;
        y := total / 2;

        while (y < total) {
            _swap(assetArray, x, y);
            x += 1;
            y += 1;
        };
    };

    private func _swap(arr: [var Nat], i: Nat, j: Nat): () {
        let temp = arr[i];
        arr[i] := arr[j];
        arr[j] := temp;
    };

    private func _buildNFTUri(uri: Text, tokenId: Nat): Text {
        let result = Text.replace(assetCanisterURI, #text(uriPattern), Nat.toText(tokenId));
        return result;
    };

    public shared(msg) func setUriPattern(pattern: Text): async Bool {
        assert((msg.caller == _minter) or _isAdmin(msg.caller));
        uriPattern := pattern;
        assert(uriPattern == pattern);
        return true;
    };

    public shared(msg) func setAssetCanisterUri(uri: Text): async Bool {
        assert((msg.caller == _minter) or _isAdmin(msg.caller));
        assetCanisterURI := uri;
        assert(assetCanisterURI == uri);
        let tokensUri = tokenURIs.entries();
        for ((k, v) in tokensUri) {
            let tokenUri = _buildNFTUri(assetCanisterURI, k);
            tokenURIs.put(k, tokenUri);
        };
        return true;
    };

    private func _isAdmin(principal: Principal): Bool {
        let value = admins.get(principal);
        switch (value) {
            case (?isAdmin) return isAdmin;
            case (null) return false;
        };
    };

    public shared(msg) func addAdmin(principal: Principal): async Bool {
        assert(msg.caller == _minter);
        admins.put(principal, true);
        return _isAdmin(principal);
    };

    //// Http


    //Internal cycle management - good general case
    public func acceptCycles(): async () {
        let available = Cycles.available();
        let accepted = Cycles.accept(available);
        assert (accepted == available);
    };
    public query func availableCycles(): async Nat {
        return Cycles.balance();
    };

    public shared(msg) func setTemplate(key: Text, template: HttpTemplate): async () {
        assert((msg.caller == _minter) or _isAdmin(msg.caller));
        templates.put(key, template);
    };

    public query(msg) func getTemplate(key: Text): async HttpTemplate {
        assert((msg.caller == _minter) or _isAdmin(msg.caller));
        return Option.unwrap(templates.get(key));
    };
    // Http 
    public query func httpRequest(request: Http.HttpRequest): async Http.HttpResponse {
        let token = Option.get(_getParam(request.url, "tokenid"), "");
        let tokenId: Nat = Nat32.toNat(_textToNat32(token));
        if (tokenId == 0 or tokenId > totalNFTs) {
            {
                status_code = 200;
                headers = [("content-type", "text/plain")];
                body = Text.encodeUtf8 (
                "Cycle Balance:   ~" # debug_show (Cycles.balance()/1000000000000) # "T\n"
                );
                streaming_strategy = null;
            };
        } else {
            let templateType = Option.get(_getParam(request.url, "type"), "");
            let httpTemplate: HttpTemplate = Option.unwrap(templates.get(templateType));

            let tokenUri = tokenURIs.get(tokenId);
            switch (tokenUri) {
                case (?uri) {
                    let assetUrl = Text.replace(uri, #text("ic0.app"), "raw.ic0.app");
                    let templateData = Text.replace(httpTemplate.template, #text(httpTemplate.pattern), assetUrl);
                    {
                        status_code = 200;
                        headers = [("Content-Type", httpTemplate.contentType)];
                        body = Text.encodeUtf8(templateData);
                        streaming_strategy = null;
                    };
                };
                case (null) {
                    _buildNotFoundHttpResponseWithBodyText("Not mint");
                };
            };
        };
    };

    private func _buildNotFoundHttpResponseWithBodyText(err: Text): Http.HttpResponse {
        {
            status_code = 404;
            headers = [];
            body = Text.encodeUtf8(err);
            streaming_strategy = null;
        };
    };

    private func _textToNat32(t: Text): Nat32 {
        var reversed: [Nat32] = [];
        for(c in t.chars()) {
            assert(Char.isDigit(c));
            reversed := Array.append([Char.toNat32(c)-48], reversed);
        };
        var total: Nat32 = 0;
        var place: Nat32  = 1;
        for(v in reversed.vals()) {
            total += (v * place);
            place := place * 10;
        };
        total;
    };

    private func _getParam(url: Text, param: Text): ?Text {
        var _s: Text = url;
        Iter.iterate<Text>(Text.split(_s, #text("/")), func(x, _i) {
            _s := x;
        });
        Iter.iterate<Text>(Text.split(_s, #text("?")), func(x, _i) {
            if (_i == 1) _s := x;
        });
        var t: ?Text = null;
        var found: Bool = false;
        Iter.iterate<Text>(Text.split(_s, #text("&")), func(x, _i) {
            if (found == false) {
                Iter.iterate<Text>(Text.split(x, #text("=")), func(y, _ii) {
                    if (_ii == 0) {
                        if (Text.equal(y, param)) found := true;
                    } else if (found == true) t := ?y;
                });
            };
        });
        return t;
    };
};