module {
    public type Header = (Text, Text);

    public type AuthToken = {};

    public type CallbackStreamingResponse = {
        content : Blob;
        token : AuthToken;
    };

    public type StreamingStrategy = {
        #Callback : {
            callback : shared AuthToken -> async CallbackStreamingResponse;
            token : AuthToken;
        };
    };

    public type HttpRequest = {
        method : Text;
        url : Text;
        headers : [Header];
        content : Blob;
    };

    public type HttpResponse = {
        statusCode : Nat16;
        headers : [Header];
        content : Blob;
        streamingStrategy : ?StreamingStrategy;
    };

    public type HttpActor = actor {
        httpRequest : query (request : HttpRequest) -> async HttpResponse;
        
        httpRequestUpdate : shared (request : HttpRequest) -> async HttpResponse;
    };
}