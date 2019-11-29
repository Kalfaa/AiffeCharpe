
namespace TimeOff

open System
open TimeOff
// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | CancelRequest of UserId * Guid
    | ValidateRequest of UserId * Guid
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | CancelRequest (userId, _) -> userId
        
// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestCancel of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestCancel request -> request
// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =                                            
    open System.Threading

    type RequestState =
        | NotCreated
        | Cancel of TimeOffRequest
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request -> request
            | Cancel request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _
            | Validated _ -> true
        
    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestCancel request -> Cancel request
        
    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let intervalOverlaps (start1: int, end1: int) (start2: int, end2: int) = end1 >= start2 && end2 >= start1

    let intervalOverlapsDay (start1: int, end1: int) (starthalf1: HalfDay, endhalf1: HalfDay) (start2: int, end2: int) (starthalf2: HalfDay, endhalf2: HalfDay) =
        end1 >= start2 &&
        end2 >= start1 &&
        start1 = start2 &&
        starthalf1 = starthalf2 ||
        end1 = end2 &&
        endhalf1 = endhalf2
    
    let overlapsWith request1 request2 =
        let StartDate1 = request1.Start.Date
        let EndDate1 = request1.End.Date
        let StartDate2 = request2.Start.Date
        let EndDate2 = request2.End.Date
        intervalOverlaps (StartDate1.Year,EndDate1.Year) (StartDate2.Year,EndDate2.Year)
            && intervalOverlaps (StartDate1.Month,EndDate1.Month) (StartDate2.Month,EndDate2.Month)
            && intervalOverlapsDay (StartDate1.Day,EndDate1.Day)(request1.Start.HalfDay,request1.End.HalfDay) (StartDate2.Day,EndDate2.Day) (request2.Start.HalfDay,request2.End.HalfDay)

    let overlapsWithAnyRequest(otherRequests: TimeOffRequest seq) request = Seq.tryFind (overlapsWith request) otherRequests <> None

    let createRequest activeUserRequests  request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        // This DateTime.Today must go away!
        elif request.Start.Date <= DateTime.Today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"
            
    let cancelRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestCancel request]
        | _ ->
            Error "Request cannot be canceled"

    let decide (userRequests: UserRequestsState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest activeUserRequests request

            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState
            | CancelRequest (_,requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    cancelRequest requestState
                     