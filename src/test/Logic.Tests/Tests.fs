module TimeOff.Tests

open Expecto
open System

let Given (events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let When (command: Command) (events: RequestEvent list, user: User) = events, user, command
let Then expected message (events: RequestEvent list, user: User, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)

    let globalState = Seq.fold evolveGlobalState Map.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide userRequestsState user command
    Expect.equal result expected message

open System

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    
    test "A request overlaps with itself" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }

    
    test "AM PM TEST" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = AM }
      }
      
      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = PM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }
      
      Expect.isFalse (Logic.overlapsWith request1 request2) "A request should overlap with istself"
    }
    
    
    test "Requests on 2 distinct days don't overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }
    test "Tuple shall overlaps" {
        Expect.isFalse (Logic.intervalOverlaps (1,1) (2,2) ) "A request should overlap with istself"
    }
  ]

[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }
    test "Overlapping Request " {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }
      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request)
      |> Then (Error "Overlapping request")  "Request Shouldn't be created"
    }
    
  ]


[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
        let request = {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
          End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
          }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }
  ]
[<Tests>]
let cancelTest =
  testList "Cancel Request" [
    test "Cancellation by employee tests" {
          let request = {
            UserId = "jdoe"
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
            End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
          }
          
      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancel request]) "The request should have been cancel"
     }
  ]
  