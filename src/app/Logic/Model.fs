
namespace TimeOff

open System
open TimeOff
// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | Validate of UserId * Guid
    | Refuse of UserId * Guid
    | Cancel of UserId * Guid
    | RefuseCancel of UserId * Guid
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | Validate (userId, _) -> userId
        | Refuse (userId, _) -> userId
        | Cancel (userId, _) -> userId
        | RefuseCancel (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestRefused of TimeOffRequest
    | RequestCancellationCreated  of TimeOffRequest
    | RequestCancellationRefused of TimeOffRequest
    | RequestCancelledByEmployee of TimeOffRequest
    | RequestCancelledByManager of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request
        | RequestValidated request
        | RequestRefused request
        | RequestCancellationCreated request
        | RequestCancellationRefused request
        | RequestCancelledByEmployee request
        | RequestCancelledByManager request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =                                            
    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest
        | Refused of TimeOffRequest
        | PendingCancellation of TimeOffRequest
        | CancellationRefused of TimeOffRequest
        | CancelledByEmployee of TimeOffRequest
        | CancelledByManager of TimeOffRequest
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request
            | Refused request
            | PendingCancellation request
            | CancellationRefused request
            | CancelledByEmployee request
            | CancelledByManager request -> request
        member this.IsActive =
            match this with
            | Refused _
            | CancelledByEmployee _
            | CancelledByManager _
            | NotCreated -> false
            | PendingValidation _
            | PendingCancellation _
            | CancellationRefused _
            | Validated _ -> true

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestRefused request -> Refused request
        | RequestCancellationCreated request -> PendingCancellation request
        | RequestCancellationRefused request -> CancellationRefused request
        | RequestCancelledByEmployee request -> CancelledByEmployee request
        | RequestCancelledByManager request -> CancelledByManager request
   
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

    let createRequest activeUserRequests request (getCurrentDate: unit -> DateTime) =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= getCurrentDate() then
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

    let cancelRequest user requestState (getCurrentDate: unit -> DateTime) =
        match requestState with
        | CancelledByManager _
        | CancelledByEmployee _ ->
            Error "Already cancelled"
        | PendingCancellation request
        | CancellationRefused request ->
            if user = Manager then
                Ok [RequestCancelledByManager request]
            else
                Error "Forbidden"
        | PendingValidation request
        | Validated request ->
            if user = Manager then
                Ok [RequestCancelledByManager request]
            elif request.Start.Date <= getCurrentDate() then
                Ok [RequestCancellationCreated request]
            else
                Ok [RequestCancelledByEmployee request] 
        | _ ->
            Error "Request cannot be cancelled"
            
    let refuseCancellationRequest requestState =
        match requestState with
        | PendingCancellation request ->
            Ok [RequestCancellationRefused request]
        | _ ->
            Error "Pending cancellation request cannot be cancelled"

    let refuseRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestRefused request]
        | _ ->
            Error "Request cannot be refused"
    
    let getWorkingDays (startDate: DateTime) (endDate: DateTime) =
        let mutable workingDays = 0.0
        let mutable currentDate = startDate 
        let numberOfDays = (startDate - endDate).Days
        
        for i = 0 to numberOfDays do
            if not (currentDate.DayOfWeek.Equals DayOfWeek.Saturday || currentDate.DayOfWeek.Equals DayOfWeek.Sunday) then
                 workingDays <- workingDays + 1.0

            currentDate <- currentDate.AddDays(1.0)

        workingDays
    
    let getTotalOffDayForThisYear : float = 25.0
        
    let getLastYearOffDayState : float = 4.5

    let countDayOffInPeriod (requests: TimeOffRequest seq) (getCurrentDate: unit -> DateTime) (before: bool) : float =
        let now = getCurrentDate()

        requests
            |> Seq.map (fun request ->
                let mutable numberOfOffDays = 0.0
                let processRequest = if before then request.Start.Date.CompareTo(now) <= 0 else request.Start.Date.CompareTo(now) >= 0

                if processRequest then
                    numberOfOffDays <- getWorkingDays request.Start.Date request.End.Date

                    if request.End.HalfDay = HalfDay.AM then
                        numberOfOffDays <- numberOfOffDays - 0.5
    
                    if request.Start.HalfDay = HalfDay.PM then
                        numberOfOffDays <- numberOfOffDays - 0.5

                numberOfOffDays
            )
            |> Seq.reduce (fun sum current -> sum + current)

    let getUsedDayOff (requests: TimeOffRequest seq) (getCurrentDate: unit -> DateTime) : float =
        countDayOffInPeriod requests getCurrentDate true

    let getPlannedDayOff (requests: TimeOffRequest seq) (getCurrentDate: unit -> DateTime) : float =
        countDayOffInPeriod requests getCurrentDate false

    let getTimeOffCurrentState (requests: TimeOffRequest seq) (getCurrentDate: unit -> DateTime) : float =
        let totalOffDayForThisYear = getTotalOffDayForThisYear
        let lastYearOffDayState = getLastYearOffDayState
        let usedDayOff = getUsedDayOff requests getCurrentDate
        let plannedDayOff = getPlannedDayOff requests getCurrentDate
        totalOffDayForThisYear + lastYearOffDayState - (usedDayOff + plannedDayOff)
    
    let getAllUserRequests (userId: UserId) (requests: TimeOffRequest seq) =
        requests
            |> Seq.filter (fun request -> request.UserId = userId)

    let decide (userRequests: UserRequestsState) (user: User) (command: Command) (getCurrentDate: unit -> DateTime) =
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

                createRequest activeUserRequests request getCurrentDate
            | Validate (_, requestId) ->
                if user <> Manager then
                    Error "Forbidden"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState user
            // Demande d'annulation d'une requête
            | Cancel (_, requestId) ->
                if user <> Manager && user <> Employee(relatedUserId) then
                    Error "Forbidden"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated                
                    cancelRequest user requestState getCurrentDate
            // Refus de la reqûete
            | Refuse (_, requestId) ->
                if user <> Manager then
                    Error "Forbidden"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated                
                    refuseRequest requestState
            // Annulation d'une demande d'annulation
            | RefuseCancel (_, requestId) ->
                if user <> Manager then
                    Error "Forbidden"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated                
                    refuseCancellationRequest requestState