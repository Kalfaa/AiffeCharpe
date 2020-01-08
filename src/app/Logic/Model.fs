
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
    | RequestCancelled of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestCancelled request -> request
// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =                                            
    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest
        | Cancelled of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request -> request
            | Cancelled request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _
            | Validated _ -> true
            | Cancelled _ -> false

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestCancelled request -> Cancelled request
        
    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)
    
    let dateAreOverlapping (start1: DateTime, end1: DateTime) (start2: DateTime, end2: DateTime) =
        DateTime.Compare(start1, end2) <= 0 && DateTime.Compare(start2, end1) <= 0
    
    let overlapsWith request1 request2 =
        let StartDate1 = request1.Start.Date
        let EndDate1 = request1.End.Date
        let EndDateHalfDay1 = request1.End.HalfDay
        let StartDate2 = request2.Start.Date
        let StartDateHalfDay2 = request2.Start.HalfDay
        let EndDate2 = request2.End.Date
        
        if dateAreOverlapping(StartDate1, EndDate1) (StartDate2, EndDate2) then
            if (DateTime.Compare(EndDate1, StartDate2) = 0 && EndDateHalfDay1 = AM && StartDateHalfDay2 = PM) then
                false
            else
                true
        else
            false

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
        | Cancelled _ ->
            Error "Already cancelled"
        | Validated request
        | PendingValidation request ->
            if request.Start.Date <= DateTime.Today then
                Error "The cancellation request is in the past"
            else
                Ok [RequestCancelled request]
        | _ ->
            Error "Request cannot be validated"

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
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated                
                cancelRequest requestState
                     