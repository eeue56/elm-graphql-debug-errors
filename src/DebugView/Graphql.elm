module DebugView.Graphql exposing (graphqlErrorToString)

{-|
@docs graphqlErrorToString
-}

import Graphql.Http exposing (HttpError(..))
import Graphql.Http.GraphqlError
import Http
import Json.Decode
import Json.Encode

{-| Turn a GraphQL Http Error into a string in English
-}
graphqlErrorToString : Graphql.Http.Error a -> String
graphqlErrorToString err =
    case err of
        Graphql.Http.GraphqlError parsedData errors ->
            let
                parsedDataMessage =
                    case parsedData of
                        Graphql.Http.GraphqlError.ParsedData _ ->
                            "Successfully got the json data: "

                        Graphql.Http.GraphqlError.UnparsedData data ->
                            "Failed to parse data: " ++ Json.Encode.encode 4 data

                errorMessages =
                    List.map .message errors
                        |> String.join "\n"
                        |> (\message -> "Error messages: " ++ message)
            in
            parsedDataMessage ++ errorMessages

        Graphql.Http.HttpError error ->
            graphqlHttpErrorToString error


badUrlMessage : String -> String
badUrlMessage url =
    "The URL " ++ url ++ " was invalid"


timeoutMessage : String
timeoutMessage =
    "Unable to reach the server, try again"


networkErrorMessage : String
networkErrorMessage =
    "Unable to reach the server, check your network connection"


badStatusMessage : Http.Metadata -> String -> String
badStatusMessage metadata response =
    "Metadata: " ++ metadataToString metadata ++ "\n\nResponse: " ++ response


badPayloadMessage : Json.Decode.Error -> String
badPayloadMessage error =
    "Failed to decode the response. Got " ++ Json.Decode.errorToString error


metadataToString : Http.Metadata -> String
metadataToString metadata =
    Json.Encode.object
        [ ( "url", Json.Encode.string metadata.url )
        , ( "statusCode", Json.Encode.int metadata.statusCode )
        , ( "statusText", Json.Encode.string metadata.statusText )
        , ( "headers", Json.Encode.dict identity Json.Encode.string metadata.headers )
        ]
        |> Json.Encode.encode 4


graphqlHttpErrorToString : Graphql.Http.HttpError -> String
graphqlHttpErrorToString error =
    case error of
        BadUrl url ->
            badUrlMessage url

        Timeout ->
            timeoutMessage

        NetworkError ->
            networkErrorMessage

        BadStatus metadata response ->
            badStatusMessage metadata response

        BadPayload jsonDecodeError ->
            badPayloadMessage jsonDecodeError
