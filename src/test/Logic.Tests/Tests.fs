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

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with itself"
    }

    
    test "AM PM TEST with different half day" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2020, 10, 1); HalfDay = AM }
      }
      
      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
      }
      
      Expect.isFalse (Logic.overlapsWith request1 request2) "A request should not overlap with itself"
    }
    
    test "AM PM TEST with same half day" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2020, 10, 5); HalfDay = AM }
      }
      
      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 10, 5); HalfDay = AM }
        End = { Date = DateTime(2020, 10, 10); HalfDay = PM }
      }
      
      Expect.isTrue (Logic.overlapsWith request1 request2) "A request should overlap with itself"
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
    
    test "Requests on 2 distinct month don't overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 11, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 11, 1); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }
    
    test "Requests on 2 distinct year don't overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2021, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2021, 10, 1); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }
    
    test "Requests on the same period overlaps" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request1 request2) "The requests overlaps"
    }
    
    test "Requests on the same period overlaps with different month" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 11, 15); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 11, 13); HalfDay = AM }
        End = { Date = DateTime(2019, 11, 25); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request1 request2) "The requests overlaps"
    }
    
    test "Requests on the same period overlaps with different year" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2020, 1, 5); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 1, 4); HalfDay = AM }
        End = { Date = DateTime(2020, 1, 8); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request1 request2) "The requests overlaps"
    }
  ]

[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }

    test "Overlapping Request " {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM } }
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
          Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
          End = { Date = DateTime(2020, 12, 27); HalfDay = PM }
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
    test "Cancellation by an employee of a request is possible" {
          let request = {
            UserId = "jdoe"
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
            End = { Date = DateTime(2020, 12, 27); HalfDay = PM }
          }
          
      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelled request]) "The request should have been cancelled"
     }
    
    test "Cancellation by employee or a previous request is impossible" {
          let request = {
            UserId = "jdoe"
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
            End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
          }
          
      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Error "The cancellation request is in the past") "The request should not have been cancelled"
     }
  ]
  