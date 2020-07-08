open System

type Timed<'a> =
    {
        Started : DateTimeOffset
        Stopped : DateTimeOffset
        Result : 'a 
    }
    member this.Duration = this.Stopped - this.Started


    module Untimed =
        let map f x = 
            { Started = x.Started; Stopped = x.Stopped; Result = f x.Result }

        let withResult newResult x = x |> map(fun _ -> newResult) 

module Timed = 
    let capture clock x = 
        let now = clock()
        {Started = now; Stopped = now ; Result = x}
    let map clock f x = 
        let result = f x.Result
        let stopped = clock()
        {Started = x.Started; Stopped = stopped; Result = result}
    let timeOn clock f x = 
        x |> capture clock |> map clock f

module Clocks = 
    let machineClock () = DateTimeOffset.Now
    let strime (x : DateTimeOffset) = x.ToString "Y"
    let acclock (start :DateTimeOffset) rate () = 
        let now = DateTimeOffset.Now
        let elapsed = now - start
        start.AddTicks(elapsed.Ticks * rate)
    
    open System.Collections.Generic

    let qlock (q : Queue<DateTimeOffset> ) = q.Dequeue
    let seqlock(l : DateTimeOffset seq ) = l |> Queue<DateTimeOffset> |> qlock


// Module 2

type todo = unit 
let todo () = ()

//State Data 

type ReadyData = Timed<todo>
type ReceivedMessageData = todo
type NoMessageData = Timed<todo>


//States

type PollingConsumer = 
| ReadyState of ReadyData
| ReceivedMessage of ReceivedMessageData
| NoMessageData of NoMessageData
| StoppedState 


// If the consumer is in stopped state it will stay in a stopped state
// This means no data associated with Stopped state -> Function degenerates to value

let transitionFromStopped : PollingConsumer = 
    StoppedState



// We can introduce a function on the fly as an input
// After we can contemplate the function and see that some states are actually timed
// TransitionFromMessage Is a higher order function because it depends on two functions shouldIdle and Idle
// If we do not supply these two functions the compiling system will complain

let transitionFromNoMessage shouldIdle idle (nm : NoMessageData) =
 if nm |> shouldIdle
 then idle () |> ReadyState
 else StoppedState

