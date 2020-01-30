module TimeOff.Tests

open Expecto
open System
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
    let getCurrentDate () = DateTime.Today
    let result = Logic.decide userRequestsState user command getCurrentDate
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
let refuseTests =
  testList "Refuse tests" [
    test "A request is refused" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 10, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 10, 27); HalfDay = PM }
      }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (Refuse ("jdoe", request.RequestId))
      |> Then (Ok [RequestRefused request]) "The request should have been refused"
    }

    test "A request can't be refused by an employee " {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 10, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 10, 27); HalfDay = PM }
      }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (Refuse ("jdoe", request.RequestId))
      |> Then (Error "Forbidden") "The request should have been forbidden"
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
      |> When (Validate ("jdoe", request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }
    
    test "A request can't be validated by an employee" {
        let request = {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
          End = { Date = DateTime(2020, 12, 27); HalfDay = PM }
          }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (Validate ("jdoe", request.RequestId))
      |> Then (Error "Forbidden") "The request should have been forbidden"
    }
  ]

[<Tests>]
let cancelTest =
  testList "Cancel Request" [
    test "Cancellation by an employee of a created request is possible" {
          let request = {
            UserId = "jdoe"
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
            End = { Date = DateTime(2020, 12, 27); HalfDay = PM }
          }
          
      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (Cancel ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelledByEmployee request]) "The request should have been cancelled"
     }
    
    test "Cancellation by an employee of a validated request is possible" {
          let request = {
            UserId = "jdoe"
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
            End = { Date = DateTime(2020, 12, 27); HalfDay = PM }
          }
          
      Given [ RequestValidated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (Cancel ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelledByEmployee request]) "The request should have been cancelled"
     } 
    
    test "Cancellation by employee of a cancelled request by himself is impossible" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM }
      }

      Given [ RequestCancelledByEmployee request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (Cancel ("jdoe", request.RequestId))
      |> Then (Error "Already cancelled") "The request should not have been cancelled"
     }
    
    test "Cancellation by employee of a cancelled request by a manager is impossible" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM }
      }

      Given [ RequestCancelledByManager request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (Cancel ("jdoe", request.RequestId))
      |> Then (Error "Already cancelled") "The request should not have been cancelled"
     }
    
    test "Cancellation by manager of a request in pending validation" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
      }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (Cancel ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelledByManager request]) "The request should have been cancelled"
     }
    
    test "Cancellation by manager of a validated past request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
      }

      Given [ RequestValidated request ]
      |> ConnectedAs Manager
      |> When (Cancel ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelledByManager request]) "The request should have been cancelled"
     }
    
    test "Cancellation by manager of a validated future request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 12, 27); HalfDay = PM }
      }

      Given [ RequestValidated request ]
      |> ConnectedAs Manager
      |> When (Cancel ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelledByManager request]) "The request should have been cancelled"
     }
    
    test "A user can ask to his manager to cancel a past request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
      }

      Given [ RequestValidated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (Cancel ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancellationCreated request]) "The request should have been cancelled"
     }
    
    test "A manager can accept a cancellation request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
      }

      Given [ RequestCancellationCreated request ]
      |> ConnectedAs Manager
      |> When (Cancel ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelledByManager request]) "The request should have been cancelled"
     }
    
    test "A manager can refuse a cancellation request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
      }

      Given [ RequestCancellationCreated request ]
      |> ConnectedAs Manager
      |> When (RefuseCancel ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancellationRefused request]) "The request should have been refused"
     }
    
    test "A manager can cancel a request after refusing a cancellation request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
      }

      Given [ RequestCancellationRefused request ]
      |> ConnectedAs Manager
      |> When (Cancel ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelledByManager request]) "The request should have been cancelled"
     }
    
    test "An employee can't accept a cancellation request" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
      }

      Given [ RequestCancellationCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (Cancel ("jdoe", request.RequestId))
      |> Then (Error "Forbidden") "The request should have been forbidden"
     }
  ]
  
  
[<Tests>]
let durationTests =
  testList "Duration tests" [
    test "Gets the correct amount of working days" {
      let result = Logic.getWorkingDays (DateTime(2020, 1, 30)) (DateTime(2020, 2, 3))
      Expect.equal result 3.0 "It should be 3 working days"
     }
    
    test "Get used day off" {
      let requests = [{
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 16); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
      }; {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 1, 6); HalfDay = AM }
        End = { Date = DateTime(2020, 1, 14); HalfDay = PM }
      }]

      let getCurrentDate () = DateTime.Today
      let result = Logic.getUsedDayOff requests getCurrentDate

      Expect.equal result 17.0 "It should be 17"
    }
    
    test "Get planned day off" {
      let requests = [{
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 16); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
      }; {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 1, 6); HalfDay = AM }
        End = { Date = DateTime(2020, 1, 14); HalfDay = PM }
      }]

      let getCurrentDate () = DateTime(2019, 6, 15)
      let result = Logic.getPlannedDayOff requests getCurrentDate

      Expect.equal result 17.0 "It should be 17"
    }
    
    test "Get current state" {
      let requests = [{
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 1, 16); HalfDay = AM }
        End = { Date = DateTime(2019, 1, 27); HalfDay = PM }
      }; {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 16); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
      }; {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 1, 6); HalfDay = AM }
        End = { Date = DateTime(2020, 1, 14); HalfDay = PM }
      }]

      let getCurrentDate () = DateTime(2019, 6, 15)
      let result = Logic.getTimeOffCurrentState requests getCurrentDate

      Expect.equal result 4.5 "It should be 4.5"
    }
  ]