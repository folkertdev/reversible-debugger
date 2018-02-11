module Types exposing (..)

import Dict exposing (Dict)
import Exts.Json.Encode
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode exposing (encode)
import Result


type alias Type =
    String


decodeType =
    Decode.string


encodeType =
    Json.Encode.string


tupleDict : Decoder value -> Decoder (Dict String value)
tupleDict decodeValue =
    let
        tuple2 value =
            Decode.map2 (,) (index 0 string) (index 1 value)
    in
        Decode.list (tuple2 decodeValue)
            |> Decode.map Dict.fromList


dict =
    tupleDict


encodeTuple3 : ( Json.Encode.Value, Json.Encode.Value, Json.Encode.Value ) -> Json.Encode.Value
encodeTuple3 ( a, b, c ) =
    Json.Encode.list [ a, b, c ]


encodeListString : List String -> Json.Encode.Value
encodeListString =
    Json.Encode.list << List.map Json.Encode.string


tuple3 : Decoder a -> Decoder b -> Decoder c -> Decoder ( a, b, c )
tuple3 a b c =
    decode (,,)
        |> required "contents" (index 0 a)
        |> required "contents" (index 1 b)
        |> required "contents" (index 2 c)


sent channelName payload =
    Sent { channelName = channelName, payload = payload }


received channelName binding variableName =
    Received { channelName = channelName, binding = binding, variableName = variableName }


receive channelName =
    Receive { channelName = channelName }


send channelName payload =
    Send { channelName = channelName, payload = payload }


decodeResult : Decoder error -> Decoder value -> Decoder (Result error value)
decodeResult errorDecoder valueDecoder =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "Ok" ->
                        decode Ok
                            |> required "contents" valueDecoder

                    "Err" ->
                        decode Err
                            |> required "contents" errorDecoder

                    _ ->
                        fail <| "Constructor not matched, got " ++ x
            )


type alias Encoder t =
    t -> Json.Encode.Value


encodeResult : Encoder error -> Encoder value -> Result error value -> Json.Encode.Value
encodeResult encodeError encodeValue x =
    case x of
        Ok y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Ok" )
                , ( "contents", encodeValue y0 )
                ]

        Err y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Err" )
                , ( "contents", encodeError y0 )
                ]


type StackAction a
    = Push a
    | Pop a


decodeStackAction : Decoder value -> Decoder (StackAction value)
decodeStackAction valueDecoder =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "Push" ->
                        decode Push
                            |> required "contents" valueDecoder

                    "Pop" ->
                        decode Pop
                            |> required "contents" valueDecoder

                    _ ->
                        fail <| "Constructor not matched, got " ++ x
            )


encodeStackAction : Encoder value -> StackAction value -> Json.Encode.Value
encodeStackAction encodeValue x =
    case x of
        Push y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Push" )
                , ( "contents", encodeValue y0 )
                ]

        Pop y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Pop" )
                , ( "contents", encodeValue y0 )
                ]


type Instruction
    = Forth PID
    | Back PID
    | Roll PID Int
    | RollSend Identifier Int
    | RollReceive Identifier Int
    | RollThread PID
    | RollVariable Identifier
    | Run
    | ListThreads
    | Store
    | Print (Result PID Identifier)
    | History (Result PID Identifier)
    | Help
    | Quit
    | SkipLets


decodeInstruction : Decoder Instruction
decodeInstruction =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "Forth" ->
                        decode Forth
                            |> required "contents" decodePID

                    "Back" ->
                        decode Back
                            |> required "contents" decodePID

                    "Roll" ->
                        decode Roll
                            |> required "contents" (index 0 decodePID)
                            |> required "contents" (index 1 int)

                    "RollSend" ->
                        decode RollSend
                            |> required "contents" (index 0 decodeIdentifier)
                            |> required "contents" (index 1 int)

                    "RollReceive" ->
                        decode RollReceive
                            |> required "contents" (index 0 decodeIdentifier)
                            |> required "contents" (index 1 int)

                    "RollThread" ->
                        decode RollThread
                            |> required "contents" decodePID

                    "RollVariable" ->
                        decode RollVariable
                            |> required "contents" decodeIdentifier

                    "Run" ->
                        decode Run

                    "ListThreads" ->
                        decode ListThreads

                    "Store" ->
                        decode Store

                    "Print" ->
                        decode Print
                            |> required "contents" (decodeResult decodePID decodeIdentifier)

                    "History" ->
                        decode History
                            |> required "contents" (decodeResult decodePID decodeIdentifier)

                    "Help" ->
                        decode Help

                    "Quit" ->
                        decode Quit

                    "SkipLets" ->
                        decode SkipLets

                    _ ->
                        fail <| "Constructor not matched, got " ++ x
            )


encodeInstruction : Instruction -> Json.Encode.Value
encodeInstruction x =
    case x of
        Forth y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Forth" )
                , ( "contents", encodePID y0 )
                ]

        Back y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Back" )
                , ( "contents", encodePID y0 )
                ]

        Roll y0 y1 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Roll" )
                , ( "contents", Json.Encode.list [ encodePID y0, Json.Encode.int y1 ] )
                ]

        RollSend y0 y1 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "RollSend" )
                , ( "contents", Json.Encode.list [ encodeIdentifier y0, Json.Encode.int y1 ] )
                ]

        RollReceive y0 y1 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "RollReceive" )
                , ( "contents", Json.Encode.list [ encodeIdentifier y0, Json.Encode.int y1 ] )
                ]

        RollThread y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "RollThread" )
                , ( "contents", encodePID y0 )
                ]

        RollVariable y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "RollVariable" )
                , ( "contents", encodeIdentifier y0 )
                ]

        Run ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Run" )
                , ( "contents", Json.Encode.list [] )
                ]

        ListThreads ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "ListThreads" )
                , ( "contents", Json.Encode.list [] )
                ]

        Store ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Store" )
                , ( "contents", Json.Encode.list [] )
                ]

        Print y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Print" )
                , ( "contents", encodeResult encodePID encodeIdentifier y0 )
                ]

        History y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "History" )
                , ( "contents", encodeResult encodePID encodeIdentifier y0 )
                ]

        Help ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Help" )
                , ( "contents", Json.Encode.list [] )
                ]

        Quit ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Quit" )
                , ( "contents", Json.Encode.list [] )
                ]

        SkipLets ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "SkipLets" )
                , ( "contents", Json.Encode.list [] )
                ]


type IntExpr
    = LiteralInt Int
    | ReferenceInt Identifier
    | IntOperator IntOperator IntExpr IntExpr


decodeIntExpr : Decoder IntExpr
decodeIntExpr =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "LiteralInt" ->
                        decode LiteralInt
                            |> required "contents" int

                    "ReferenceInt" ->
                        decode ReferenceInt
                            |> required "contents" decodeIdentifier

                    "IntOperator" ->
                        decode IntOperator
                            |> required "contents" (index 0 decodeIntOperator)
                            |> required "contents" (index 1 decodeIntExpr)
                            |> required "contents" (index 2 decodeIntExpr)

                    _ ->
                        fail <| "Constructor not matched, got " ++ x
            )


encodeIntExpr : IntExpr -> Json.Encode.Value
encodeIntExpr x =
    case x of
        LiteralInt y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "LiteralInt" )
                , ( "contents", Json.Encode.int y0 )
                ]

        ReferenceInt y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "ReferenceInt" )
                , ( "contents", encodeIdentifier y0 )
                ]

        IntOperator y0 y1 y2 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "IntOperator" )
                , ( "contents", Json.Encode.list [ encodeIntOperator y0, encodeIntExpr y1, encodeIntExpr y2 ] )
                ]


type BoolExpr
    = LiteralBool Bool
    | ReferenceBool Identifier
    | BoolOperator BooleanOperator IntExpr IntExpr


decodeBoolExpr : Decoder BoolExpr
decodeBoolExpr =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "LiteralBool" ->
                        decode LiteralBool
                            |> required "contents" bool

                    "ReferenceBool" ->
                        decode ReferenceBool
                            |> required "contents" decodeIdentifier

                    "BoolOperator" ->
                        decode BoolOperator
                            |> required "contents" (index 0 decodeBooleanOperator)
                            |> required "contents" (index 1 decodeIntExpr)
                            |> required "contents" (index 2 decodeIntExpr)

                    _ ->
                        fail <| "Constructor not matched, got " ++ x
            )


encodeBoolExpr : BoolExpr -> Json.Encode.Value
encodeBoolExpr x =
    case x of
        LiteralBool y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "LiteralBool" )
                , ( "contents", Json.Encode.bool y0 )
                ]

        ReferenceBool y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "ReferenceBool" )
                , ( "contents", encodeIdentifier y0 )
                ]

        BoolOperator y0 y1 y2 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "BoolOperator" )
                , ( "contents", Json.Encode.list [ encodeBooleanOperator y0, encodeIntExpr y1, encodeIntExpr y2 ] )
                ]


type alias ReplState =
    { context : Context
    , threadState : ThreadState
    }


decodeReplState : Decoder ReplState
decodeReplState =
    decode ReplState
        |> required "context" decodeContext
        |> required "threadState" decodeThreadState


encodeReplState : ReplState -> Json.Encode.Value
encodeReplState x =
    Json.Encode.object
        [ ( "context", encodeContext x.context )
        , ( "threadState", encodeThreadState x.threadState )
        ]


type alias Context =
    { bindings : Dict Identifier ( Participant, Value )
    , variableCount : Int
    , channels : Dict Identifier ( Participant, Queue )
    , threads : Dict (List Int) Int
    , localTypeStates : Dict String LocalTypeState
    , globalType : GlobalType
    }


tuple : Decoder a -> Decoder b -> Decoder ( a, b )
tuple a b =
    map2 (,) (index 0 a) (index 1 b)


dict2 : Decoder comparable -> Decoder value -> Decoder (Dict comparable value)
dict2 key value =
    Decode.list (tuple key value)
        |> Decode.map Dict.fromList


decodeContext : Decoder Context
decodeContext =
    decode Context
        |> required "bindings" (dict2 decodeIdentifier (tuple decodeParticipant decodeValue))
        |> required "variableCount" int
        |> required "channels" (dict2 decodeIdentifier (tuple decodeParticipant decodeQueue))
        |> required "threads" (map Dict.fromList (list (map2 (,) (index 0 (list int)) (index 1 int))))
        |> required "localTypeStates" (dict2 (Decode.map unParticipant decodeParticipant) decodeLocalTypeState)
        |> required "globalType" decodeGlobalType


encodeContext : Context -> Json.Encode.Value
encodeContext x =
    Json.Encode.object
        [ ( "bindings", Exts.Json.Encode.dict encodeIdentifier (Exts.Json.Encode.tuple2 encodeParticipant encodeValue) x.bindings )
        , ( "variableCount", Json.Encode.int x.variableCount )
        , ( "channels", Exts.Json.Encode.dict encodeIdentifier (Exts.Json.Encode.tuple2 encodeParticipant encodeQueue) x.channels )
        , ( "threads", Exts.Json.Encode.dict (Json.Encode.list << List.map Json.Encode.int) Json.Encode.int x.threads )
        , ( "localTypeStates", Exts.Json.Encode.dict (encodeParticipant << Participant) encodeLocalTypeState x.localTypeStates )
        , ( "globalType", encodeGlobalType x.globalType )
        ]


type alias GlobalType =
    { parameters : List Identifier
    , atoms : List GlobalAtom
    }


decodeGlobalType : Decoder GlobalType
decodeGlobalType =
    decode GlobalType
        |> required "parameters" (list decodeIdentifier)
        |> required "atoms" (list decodeGlobalAtom)


encodeGlobalType : GlobalType -> Json.Encode.Value
encodeGlobalType x =
    Json.Encode.object
        [ ( "parameters", (Json.Encode.list << List.map encodeIdentifier) x.parameters )
        , ( "atoms", (Json.Encode.list << List.map encodeGlobalAtom) x.atoms )
        ]


type alias GlobalAtom =
    { sender : Identifier
    , receiver : Identifier
    , tipe : String
    }


decodeGlobalAtom : Decoder GlobalAtom
decodeGlobalAtom =
    decode GlobalAtom
        |> required "sender" decodeIdentifier
        |> required "receiver" decodeIdentifier
        |> required "tipe" string


encodeGlobalAtom : GlobalAtom -> Json.Encode.Value
encodeGlobalAtom x =
    Json.Encode.object
        [ ( "sender", encodeIdentifier x.sender )
        , ( "receiver", encodeIdentifier x.receiver )
        , ( "tipe", Json.Encode.string x.tipe )
        ]


type alias OtherThreads =
    { active : Dict (List Int) Thread
    , inactive : Dict (List Int) Thread
    , blocked : Dict (List Int) Thread
    , filtered : Dict (List Int) Thread
    , uninitialized : Dict (List Int) Thread
    }


decodeOtherThreads : Decoder OtherThreads
decodeOtherThreads =
    decode OtherThreads
        |> required "active" (map Dict.fromList (list (map2 (,) (index 0 (list int)) (index 1 decodeThread))))
        |> required "inactive" (map Dict.fromList (list (map2 (,) (index 0 (list int)) (index 1 decodeThread))))
        |> required "blocked" (map Dict.fromList (list (map2 (,) (index 0 (list int)) (index 1 decodeThread))))
        |> required "filtered" (map Dict.fromList (list (map2 (,) (index 0 (list int)) (index 1 decodeThread))))
        |> required "uninitialized" (map Dict.fromList (list (map2 (,) (index 0 (list int)) (index 1 decodeThread))))


encodeOtherThreads : OtherThreads -> Json.Encode.Value
encodeOtherThreads x =
    Json.Encode.object
        [ ( "active", Exts.Json.Encode.dict (Json.Encode.list << List.map Json.Encode.int) encodeThread x.active )
        , ( "inactive", Exts.Json.Encode.dict (Json.Encode.list << List.map Json.Encode.int) encodeThread x.inactive )
        , ( "blocked", Exts.Json.Encode.dict (Json.Encode.list << List.map Json.Encode.int) encodeThread x.blocked )
        , ( "filtered", Exts.Json.Encode.dict (Json.Encode.list << List.map Json.Encode.int) encodeThread x.filtered )
        , ( "uninitialized", Exts.Json.Encode.dict (Json.Encode.list << List.map Json.Encode.int) encodeThread x.uninitialized )
        ]


type ThreadState
    = Running Thread OtherThreads
    | Stuck OtherThreads


decodeThreadState : Decoder ThreadState
decodeThreadState =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "Running" ->
                        decode Running
                            |> required "contents" (index 0 decodeThread)
                            |> required "contents" (index 1 decodeOtherThreads)

                    "Stuck" ->
                        decode Stuck
                            |> required "contents" decodeOtherThreads

                    _ ->
                        fail <| "ThreadState: Constructor not matched, got " ++ x
            )


encodeThreadState : ThreadState -> Json.Encode.Value
encodeThreadState x =
    case x of
        Running y0 y1 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Running" )
                , ( "contents", Json.Encode.list [ encodeThread y0, encodeOtherThreads y1 ] )
                ]

        Stuck y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Stuck" )
                , ( "contents", encodeOtherThreads y0 )
                ]


type alias Thread =
    { pid : PID
    , actor : Actor
    , history : List History
    , program : List Program
    }


decodeThread : Decoder Thread
decodeThread =
    decode Thread
        |> required "pid" decodePID
        |> required "actor" decodeActor
        |> required "history" (list decodeHistory)
        |> required "program" (list decodeProgram)


encodeThread : Thread -> Json.Encode.Value
encodeThread x =
    Json.Encode.object
        [ ( "pid", encodePID x.pid )
        , ( "actor", encodeActor x.actor )
        , ( "history", (Json.Encode.list << List.map encodeHistory) x.history )
        , ( "program", (Json.Encode.list << List.map encodeProgram) x.program )
        ]


type alias Identifier =
    String


decodeIdentifier : Decoder Identifier
decodeIdentifier =
    decode identity
        |> required "unwrap" string


encodeIdentifier : Identifier -> Json.Encode.Value
encodeIdentifier x =
    Json.Encode.object
        [ ( "unwrap", Json.Encode.string x ) ]


type PID
    = PID (List Int)


decodePID : Decoder PID
decodePID =
    list int
        |> map PID


encodePID : PID -> Json.Encode.Value
encodePID x =
    case x of
        PID y0 ->
            (Json.Encode.list << List.map Json.Encode.int) y0


type alias Actor =
    List Participant


decodeActor : Decoder Actor
decodeActor =
    list decodeParticipant


encodeActor : Encoder Actor
encodeActor participants =
    (Json.Encode.list << List.map encodeParticipant) participants


type History
    = Skipped
    | Composed
    | Sent
        { channelName : Identifier
        , payload : Identifier
        }
    | Received
        { channelName : Identifier
        , binding : Identifier
        , variableName : Identifier
        }
    | CreatedVariable Identifier
    | CreatedChannel Identifier
    | CalledProcedure Identifier (List Identifier)
    | SpawnedThread Actor PID
    | BranchedOn BoolExpr Bool Program
    | AssertedOn BoolExpr
    | ChangedActor (StackAction Participant)


decodeHistory : Decoder History
decodeHistory =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "Skipped" ->
                        decode Skipped

                    "Composed" ->
                        decode Composed

                    "Sent" ->
                        decode sent
                            |> required "channelName" decodeIdentifier
                            |> required "payload" decodeIdentifier

                    "Received" ->
                        decode received
                            |> required "channelName" decodeIdentifier
                            |> required "binding" decodeIdentifier
                            |> required "variableName" decodeIdentifier

                    "CreatedVariable" ->
                        decode CreatedVariable
                            |> required "contents" decodeIdentifier

                    "CreatedChannel" ->
                        decode CreatedChannel
                            |> required "contents" decodeIdentifier

                    "CalledProcedure" ->
                        decode CalledProcedure
                            |> required "contents" (index 0 decodeIdentifier)
                            |> required "contents" (index 1 (list decodeIdentifier))

                    "SpawnedThread" ->
                        decode SpawnedThread
                            |> required "contents" (index 0 decodeActor)
                            |> required "contents" (index 1 decodePID)

                    "BranchedOn" ->
                        decode BranchedOn
                            |> required "contents" (index 0 decodeBoolExpr)
                            |> required "contents" (index 1 bool)
                            |> required "contents" (index 2 decodeProgram)

                    "AssertedOn" ->
                        decode AssertedOn
                            |> required "contents" decodeBoolExpr

                    "ChangedActor" ->
                        decode ChangedActor
                            |> required "contents" (decodeStackAction decodeParticipant)

                    _ ->
                        fail <| "History: Constructor not matched, got " ++ x
            )


encodeHistory : History -> Json.Encode.Value
encodeHistory x =
    case x of
        Skipped ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Skipped" )
                , ( "contents", Json.Encode.list [] )
                ]

        Composed ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Composed" )
                , ( "contents", Json.Encode.list [] )
                ]

        Sent x ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Sent" )
                , ( "channelName", encodeIdentifier x.channelName )
                , ( "payload", encodeIdentifier x.payload )
                ]

        Received x ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Received" )
                , ( "channelName", encodeIdentifier x.channelName )
                , ( "binding", encodeIdentifier x.binding )
                , ( "variableName", encodeIdentifier x.variableName )
                ]

        CreatedVariable y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "CreatedVariable" )
                , ( "contents", encodeIdentifier y0 )
                ]

        CreatedChannel y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "CreatedChannel" )
                , ( "contents", encodeIdentifier y0 )
                ]

        CalledProcedure y0 y1 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "CalledProcedure" )
                , ( "contents", Json.Encode.list [ encodeIdentifier y0, (Json.Encode.list << List.map encodeIdentifier) y1 ] )
                ]

        SpawnedThread y0 y1 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "SpawnedThread" )
                , ( "contents", Json.Encode.list [ encodeActor y0, encodePID y1 ] )
                ]

        BranchedOn y0 y1 y2 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "BranchedOn" )
                , ( "contents", Json.Encode.list [ encodeBoolExpr y0, Json.Encode.bool y1, encodeProgram y2 ] )
                ]

        AssertedOn y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "AssertedOn" )
                , ( "contents", encodeBoolExpr y0 )
                ]

        ChangedActor y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "ChangedActor" )
                , ( "contents", encodeStackAction encodeParticipant y0 )
                ]


type Value
    = Receive
        { channelName : Identifier
        }
    | Procedure (List Identifier) Program
    | Port
    | VInt IntExpr
    | VBool BoolExpr
    | VUnit


decodeValue : Decoder Value
decodeValue =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "Receive" ->
                        decode receive
                            |> required "channelName" decodeIdentifier

                    "Procedure" ->
                        decode Procedure
                            |> required "contents" (index 0 (list decodeIdentifier))
                            |> required "contents" (index 1 decodeProgram)

                    "Port" ->
                        decode Port

                    "VInt" ->
                        decode VInt
                            |> required "contents" decodeIntExpr

                    "VBool" ->
                        decode VBool
                            |> required "contents" decodeBoolExpr

                    "VUnit" ->
                        decode VUnit

                    _ ->
                        fail <| "Value: Constructor not matched, got " ++ x
            )


encodeValue : Value -> Json.Encode.Value
encodeValue x =
    case x of
        Receive x ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Receive" )
                , ( "channelName", encodeIdentifier x.channelName )
                ]

        Procedure y0 y1 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Procedure" )
                , ( "contents", Json.Encode.list [ (Json.Encode.list << List.map encodeIdentifier) y0, encodeProgram y1 ] )
                ]

        Port ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Port" )
                , ( "contents", Json.Encode.list [] )
                ]

        VInt y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "VInt" )
                , ( "contents", encodeIntExpr y0 )
                ]

        VBool y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "VBool" )
                , ( "contents", encodeBoolExpr y0 )
                ]

        VUnit ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "VUnit" )
                  -- , ( "contents", encodeBoolExpr y0 )
                ]


type BooleanOperator
    = Equal
    | LessThan
    | GreaterThan
    | LessThanEqual
    | GreaterThanEqual


decodeBooleanOperator : Decoder BooleanOperator
decodeBooleanOperator =
    string
        |> andThen
            (\x ->
                case x of
                    "Equal" ->
                        decode Equal

                    "LessThan" ->
                        decode LessThan

                    "GreaterThan" ->
                        decode GreaterThan

                    "LessThanEqual" ->
                        decode LessThanEqual

                    "GreaterThanEqual" ->
                        decode GreaterThanEqual

                    _ ->
                        fail <| "BooleanOperator: Constructor not matched, got " ++ x
            )


encodeBooleanOperator : BooleanOperator -> Json.Encode.Value
encodeBooleanOperator x =
    case x of
        Equal ->
            Json.Encode.string "Equal"

        LessThan ->
            Json.Encode.string "LessThan"

        GreaterThan ->
            Json.Encode.string "GreaterThan"

        LessThanEqual ->
            Json.Encode.string "LessThanEqual"

        GreaterThanEqual ->
            Json.Encode.string "GreaterThanEqual"


type IntOperator
    = Add
    | Subtract
    | Divide
    | Multiply


decodeIntOperator : Decoder IntOperator
decodeIntOperator =
    string
        |> andThen
            (\x ->
                case x of
                    "Add" ->
                        decode Add

                    "Subtract" ->
                        decode Subtract

                    "Divide" ->
                        decode Divide

                    "Multiply" ->
                        decode Multiply

                    _ ->
                        fail <| "IntOperator: Constructor not matched, got " ++ x
            )


encodeIntOperator : IntOperator -> Json.Encode.Value
encodeIntOperator x =
    case x of
        Add ->
            Json.Encode.string "Add"

        Subtract ->
            Json.Encode.string "Subtract"

        Divide ->
            Json.Encode.string "Divide"

        Multiply ->
            Json.Encode.string "Multiply"


type Program
    = Sequence Program Program
    | Let Identifier Value Program
    | If BoolExpr Program Program
    | SpawnThread Actor Program
    | Skip
    | Apply Identifier (List Identifier)
    | Send
        { channelName : Identifier
        , payload : Identifier
        }
    | Assert BoolExpr
    | ChangeActor (StackAction Participant)


decodeProgram : Decoder Program
decodeProgram =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "Sequence" ->
                        decode Sequence
                            |> required "contents" (index 0 decodeProgram)
                            |> required "contents" (index 1 decodeProgram)

                    "Let" ->
                        decode Let
                            |> required "contents" (index 0 decodeIdentifier)
                            |> required "contents" (index 1 decodeValue)
                            |> required "contents" (index 2 decodeProgram)

                    "If" ->
                        decode If
                            |> required "contents" (index 0 decodeBoolExpr)
                            |> required "contents" (index 1 decodeProgram)
                            |> required "contents" (index 2 decodeProgram)

                    "SpawnThread" ->
                        decode SpawnThread
                            |> required "contents" (index 0 decodeActor)
                            |> required "contents" (index 1 decodeProgram)

                    "Skip" ->
                        decode Skip

                    "Apply" ->
                        decode Apply
                            |> required "contents" (index 0 decodeIdentifier)
                            |> required "contents" (index 1 (list decodeIdentifier))

                    "Send" ->
                        decode send
                            |> required "channelName" decodeIdentifier
                            |> required "payload" decodeIdentifier

                    "Assert" ->
                        decode Assert
                            |> required "contents" decodeBoolExpr

                    "ChangeActor" ->
                        decode ChangeActor
                            |> required "contents" (decodeStackAction decodeParticipant)

                    _ ->
                        fail <| "Program: Constructor not matched, got " ++ x
            )


encodeProgram : Program -> Json.Encode.Value
encodeProgram x =
    case x of
        Sequence y0 y1 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Sequence" )
                , ( "contents", Json.Encode.list [ encodeProgram y0, encodeProgram y1 ] )
                ]

        Let y0 y1 y2 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Let" )
                , ( "contents", Json.Encode.list [ encodeIdentifier y0, encodeValue y1, encodeProgram y2 ] )
                ]

        If y0 y1 y2 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "If" )
                , ( "contents", Json.Encode.list [ encodeBoolExpr y0, encodeProgram y1, encodeProgram y2 ] )
                ]

        SpawnThread y0 y1 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "SpawnThread" )
                , ( "contents", Json.Encode.list [ encodeActor y0, encodeProgram y1 ] )
                ]

        Skip ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Skip" )
                , ( "contents", Json.Encode.list [] )
                ]

        Apply y0 y1 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Apply" )
                , ( "contents", Json.Encode.list [ encodeIdentifier y0, (Json.Encode.list << List.map encodeIdentifier) y1 ] )
                ]

        Send x ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Send" )
                , ( "channelName", encodeIdentifier x.channelName )
                , ( "payload", encodeIdentifier x.payload )
                ]

        Assert y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Assert" )
                , ( "contents", encodeBoolExpr y0 )
                ]

        ChangeActor y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "ChangeActor" )
                , ( "contents", encodeStackAction encodeParticipant y0 )
                ]


type alias Queue =
    { past : List QueueHistory
    , items : List Item
    }


decodeQueue : Decoder Queue
decodeQueue =
    decode Queue
        |> required "past" (list decodeQueueHistory)
        |> required "items" (list decodeItem)


encodeQueue : Queue -> Json.Encode.Value
encodeQueue x =
    Json.Encode.object
        [ ( "past", (Json.Encode.list << List.map encodeQueueHistory) x.past )
        , ( "items", (Json.Encode.list << List.map encodeItem) x.items )
        ]


type alias Item =
    { sender : Participant
    , receiver : Participant
    , type_ : String
    , payload : Value
    }


decodeItem : Decoder Item
decodeItem =
    decode Item
        |> required "sender" decodeParticipant
        |> required "receiver" decodeParticipant
        |> required "type_" string
        |> required "payload" decodeValue


encodeItem : Item -> Json.Encode.Value
encodeItem x =
    Json.Encode.object
        [ ( "sender", encodeParticipant x.sender )
        , ( "receiver", encodeParticipant x.receiver )
        , ( "type_", Json.Encode.string x.type_ )
        , ( "payload", encodeValue x.payload )
        ]


type QueueHistory
    = Added Participant Type
    | Removed Participant Type


decodeQueueHistory : Decoder QueueHistory
decodeQueueHistory =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "Added" ->
                        decode Added
                            |> required "contents" (index 0 decodeParticipant)
                            |> required "contents" (index 1 decodeType)

                    "Removed" ->
                        decode Removed
                            |> required "contents" (index 0 decodeParticipant)
                            |> required "contents" (index 1 decodeType)

                    _ ->
                        fail <| "Constructor not matched, got " ++ x
            )


encodeQueueHistory : QueueHistory -> Json.Encode.Value
encodeQueueHistory x =
    case x of
        Added y0 tipe ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Added" )
                , ( "contents", Json.Encode.list [ encodeParticipant y0, encodeType tipe ] )
                ]

        Removed y0 tipe ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Removed" )
                , ( "contents", Json.Encode.list [ encodeParticipant y0, encodeType tipe ] )
                ]


type alias LocalTypeState =
    { participant : Participant
    , state : ExhaustableZipper (LocalAtom String)
    }


decodeLocalTypeState : Decoder LocalTypeState
decodeLocalTypeState =
    decode LocalTypeState
        |> required "participant" decodeParticipant
        |> required "state" (decodeExhaustableZipper (decodeLocalAtom string))


encodeLocalTypeState : LocalTypeState -> Json.Encode.Value
encodeLocalTypeState x =
    Json.Encode.object
        [ ( "participant", encodeParticipant x.participant )
        , ( "state", encodeExhaustableZipper (encodeLocalAtom Json.Encode.string) x.state )
        ]


type ExhaustableZipper a
    = Empty
    | Zipper ( List a, a, List a )
    | ExhaustedForward (List a) a
    | ExhaustedBackward a (List a)


decodeExhaustableZipper : Decoder value -> Decoder (ExhaustableZipper value)
decodeExhaustableZipper decodeValue =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "Empty" ->
                        decode Empty

                    "Zipper" ->
                        Decode.map (\( a, b, c ) -> Zipper ( a, b, c )) (tuple3 (list decodeValue) decodeValue (list decodeValue))

                    "ExhaustedForward" ->
                        decode ExhaustedForward
                            |> required "contents" (index 0 (list decodeValue))
                            |> required "contents" (index 1 decodeValue)

                    "ExhaustedBackward" ->
                        decode ExhaustedBackward
                            |> required "contents" (index 0 decodeValue)
                            |> required "contents" (index 1 (list decodeValue))

                    _ ->
                        fail <| "Constructor not matched, got " ++ x
            )


encodeExhaustableZipper : Encoder a -> ExhaustableZipper a -> Json.Encode.Value
encodeExhaustableZipper encodeValue x =
    case x of
        Empty ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Empty" )
                , ( "contents", Json.Encode.list [] )
                ]

        Zipper ( a, b, c ) ->
            let
                encodeList decodeV =
                    Json.Encode.list << List.map decodeV
            in
                Json.Encode.object
                    [ ( "tag", Json.Encode.string "Zipper" )
                    , ( "contents", encodeTuple3 ( encodeList encodeValue a, encodeValue b, encodeList encodeValue c ) )
                    ]

        ExhaustedForward previous current ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "ExhaustedForward" )
                , ( "contents", Json.Encode.list [ Json.Encode.list <| List.map encodeValue previous, encodeValue current ] )
                ]

        ExhaustedBackward current next ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "ExhaustedBackward" )
                , ( "contents", Json.Encode.list [ encodeValue current, Json.Encode.list <| List.map encodeValue next ] )
                ]


type LocalAtom a
    = LocalAtomSend
        { receiver : Identifier
        , type_ : a
        }
    | LocalAtomReceive
        { sender : Identifier
        , type_ : a
        }
    | Choice (LocalAtom a) (LocalAtom a)
    | Select (LocalAtom a) (LocalAtom a)
    | End


decodeLocalAtom : Decoder a -> Decoder (LocalAtom a)
decodeLocalAtom decodeType =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "Send" ->
                        decode (\receiver type_ -> LocalAtomSend { receiver = receiver, type_ = type_ })
                            |> required "receiver" decodeIdentifier
                            |> required "type_" decodeType

                    "Receive" ->
                        decode (\sender type_ -> LocalAtomReceive { sender = sender, type_ = type_ })
                            |> required "sender" decodeIdentifier
                            |> required "type_" decodeType

                    "Choice" ->
                        decode Choice
                            |> required "content" (index 0 (lazy (\_ -> decodeLocalAtom decodeType)))
                            |> required "content" (index 1 (lazy (\_ -> decodeLocalAtom decodeType)))

                    "Select" ->
                        decode Select
                            |> required "content" (index 0 (lazy (\_ -> decodeLocalAtom decodeType)))
                            |> required "content" (index 1 (lazy (\_ -> decodeLocalAtom decodeType)))

                    "End" ->
                        decode End

                    _ ->
                        fail <| "Constructor not matched, got " ++ x
            )


encodeLocalAtom : Encoder t -> LocalAtom t -> Json.Encode.Value
encodeLocalAtom encodeType x =
    case x of
        LocalAtomSend x ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Send" )
                , ( "receiver", encodeIdentifier x.receiver )
                , ( "type_", encodeType x.type_ )
                ]

        LocalAtomReceive x ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Receive" )
                , ( "sender", encodeIdentifier x.sender )
                , ( "type_", encodeType x.type_ )
                ]

        Choice left right ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Choice" )
                , ( "contents", Json.Encode.list [ encodeLocalAtom encodeType left, encodeLocalAtom encodeType right ] )
                ]

        Select left right ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Select" )
                , ( "contents", Json.Encode.list [ encodeLocalAtom encodeType left, encodeLocalAtom encodeType right ] )
                ]

        End ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "End" ) ]


type Participant
    = Participant Identifier


unParticipant : Participant -> String
unParticipant (Participant s) =
    s


decodeParticipant : Decoder Participant
decodeParticipant =
    decodeIdentifier
        |> map Participant


encodeParticipant : Participant -> Json.Encode.Value
encodeParticipant x =
    case x of
        Participant y0 ->
            encodeIdentifier y0
