
namespace TimeOff

open System
open TimeOff
// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | CancelRequest of UserId * Guid
    | DenyRequest of UserId * Guid
    | CancelCancellationRequest of UserId * Guid
    | ValidateRequest of UserId * Guid
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | CancelRequest (userId, _) -> userId
        | CancelCancellationRequest (userId, _) -> userId
        | DenyRequest (userId, _) -> userId
        
// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestCancelledByUser of TimeOffRequest
    | RequestCancelledByManager of TimeOffRequest
    | RequestCancellationCreated  of TimeOffRequest
    | RequestCancelCancellation of TimeOffRequest
    | RequestDenied of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestCancelledByUser request -> request
        | RequestCancelledByManager request -> request
        | RequestCancellationCreated request -> request
        | RequestCancelCancellation request -> request
        | RequestDenied request -> request
// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =                                            
    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest
        | Denied of TimeOffRequest
        | Cancelled of TimeOffRequest
        | Cancellation of TimeOffRequest
        | CancelCancellation of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request
            | Cancelled request
            | Denied request
            | Cancellation request
            | CancelCancellation request -> request
        member this.IsActive =
            match this with
            | Cancelled _
            | Denied _
            | Cancellation _
            | CancelCancellation _
            | NotCreated -> false
            | PendingValidation _
            | Validated _ -> true

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestCancelledByUser request -> Cancelled request
        | RequestCancelledByManager request -> Cancelled request
        | RequestDenied request -> Denied request
        | RequestCancellationCreated request -> Cancellation request
        | RequestCancelCancellation request -> CancelCancellation request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)
    
    let dateAreOverlapping (start1: DateTime, end1: DateTime) (start2: DateTime, end2: DateTime) =
        DateTime.Compare(start1, end2) <= 0 && DateTime.Compare(start2, end1) <= 0
    
    let overlapsWith request1 request2 =
        if request1.UserId <> request2.UserId then
            false
        else
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

    let createRequest activeUserRequests  request beforeDate =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= beforeDate then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState user =
        match requestState with
        | PendingValidation request ->
            if user <> Manager then
                    Error "Unauthorized"    
            else
                Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"

    let cancelRequest user requestState relatedUserId =
        match requestState with
        | Cancelled _ ->
            Error "Already cancelled"
        | PendingValidation request
        | Validated request ->
            if user = Manager then
                Ok [RequestCancelledByManager request]
            elif request.Start.Date <= DateTime.Today then
                Ok [RequestCancellationCreated request]
            else
                Ok [RequestCancelledByUser request] 
        | _ ->
            Error "Request cannot be cancelled"
            
    let cancelCancellationRequest requestState =
        match requestState with
        | Cancellation request ->
            Ok [RequestCancelCancellation request]
        | _ ->
            Error "Pending cancellation request cannot be cancel"

    let denyRequest requestState =
        match requestState with
        | PendingValidation request
        | Denied request ->
            Ok [RequestDenied request]
        | _ ->
            Error "Request cannot be denied"
    
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

                createRequest activeUserRequests request DateTime.Today
            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Forbidden"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState user
            // Demande d'annulation d'une requête
            | CancelRequest (_, requestId) ->
                if user <> Manager && user <> Employee(relatedUserId) then
                    Error "Forbidden"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated                
                    cancelRequest user requestState relatedUserId
            // Refus de la reqûete
            | DenyRequest (_, requestId) ->
                if user <> Manager then
                    Error "Forbidden"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated                
                    denyRequest requestState
            // Annulation d'une demande d'annulation
            | CancelCancellationRequest (_, requestId) ->
                if user <> Manager then
                    Error "Forbidden"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated                
                    cancelCancellationRequest requestState