module Examples exposing (asynchAnd, threeBuyer)


threeBuyer =
    """global type ThreeBuyer A B C V =
        A -> V: <title>.V -> {A, B} : <price>.A -> B : <share>.
            B -> {A, V} : <OK>.
            B -> C: <share>. B -> C : <thunk>.
            B -> V : <address>. V -> B : <date>. end

session ThreeBuyer Alice Bob Carol Vendor where

let d = port in

// A
thread Alice where
    let title = 42 in
        {send d title} ;
        let price = {receive d} in
            {send d share};
            let ok = {receive d} in
                skip
            end
        end
    end
end;

// B
thread Bob where
    let ok = 1 in
        let address = 8 in
            let price = {receive d} in
                let share = {receive d} in
                    let remainder = proc { x }
                            {send d address};
                            let date = {receive d} in
                                skip
                            end
                         end
                    in
                        {send d ok};
                        {send d ok};
                        {send d share};
                        {send d remainder}
                    end
                end
            end
        end
    end
end;

thread Carol where
    let unit = 0 in
        let share = {receive d} in
            let remainder = {receive d} in
                {remainder unit}
            end
        end
    end
end;



thread Vendor where
    let price = 5 in
        let date = 0 in
            let title = {receive d} in
                // send to A
                {send d price} ;
                // send to B
                {send d price} ;
                let ok = {receive d} in
                    let address = {receive d} in
                        {send d date}
                    end
                end
            end
        end
    end
end

// channel d
end

// session where block
end
"""


asynchAnd =
    """global type ThreeBuyer A B C V =
        A -> V: <title>.V -> {A, B} : <price>.A -> B : <share>.
            B -> {A, V} : <OK>.
            B -> C: <share>. B -> C : <thunk>.
            B -> V : <address>. V -> B : <date>. end

session ThreeBuyer A B C D where

let t = true in
let f = false in
let c = 10 in
let p = 15 in
let l = 2 in
let num = 2 in
let input = port in
let result = port in
let outAdd = port in
let outCr = port in
let outIt = port in

// procedure for performing an asynchronous variant of the logic AND among the results of the different checks
let asynchAnd = proc {n inp out}
                if (n>0) then let k={receive inp} in  //"inp" is the input channel of the procedure
                              let v={receive k} in  //k is the channel for receiving the operand
                              let m = n-1 in
                              if v then {asynchAnd m inp out}
                                                    //if v is true, the next operand is checked
                              else {send out f} end //else the AND is false
                              end // closing "let m=.."
                              end // closing "let v=.."
                              end // closing "let k=.."
                else {send out t} end //if n=0 all the operands have been processed and are true
                end in // closing of procedure "asynchAnd"

// procedure for proposing a loan to the client, when the credit check fails
let proposeLoan = proc {out}
                  let n = c+l in
                  if (n>p) then {send out t} else {send out f} end
                  end // closing "let n .."
                  end in //closing of procedure "proposeLoan"

// procedure for checking the credit
let checkCr = proc {out}
              if (c>p) then {send out t} else {proposeLoan out} end
              end in

// procedure for checking the address
let checkAdd = proc {out} {send out t} end in

// procedure for checking the availability of the item
let checkIt = proc {out} {send out t} end in

   // first thread invoking the "asynchAnd" among "num" operands and
    // waiting for inputs on channel "input" and sending result on channel "result"
    thread  A where {asynchAnd num input result} end;

    // second thread sending on "input" the channel "outAdd" and then invoking "checkAdd"
    // channel "outAdd" is for communicating the result to the "asynchAnd" operation.
    thread B where {send input outAdd};{checkAdd outAdd} end;

    // third thread sending on "input" the channel "outIt" and then invoking "checkIt"
    // channel "outIt" is for communicating the result to the "asynchAnd" operation.
    thread C where {send input outIt};{checkIt outIt} end;

    // fourth thread sending on "input" to "asynchAnd" the channel "outCr".
    // Then invoking "checkCr" with channels "outCr" for
    // communicating with "asynchAnd".
    thread D where {send input outCr};{checkCr outCr} end


end // closing of "let checkIt=.."
end // closing of "let checkAdd=.."
end // closing of "let checkCr=.."
end // closing of "let proposeLoan=.."
end // closing of "let asynchAnd=.."
end // closing of "let outIt=.."
end // closing of "let outCr=.."
end // closing of "let outAdd=.."
end // closing of "let result=.."
end // closing of "let input=.."
end // closing of "let num=.."
end // closing of "let l=.."
end // closing of "let p=.."
end // closing of "let c=.."
end // closing of "let f=.."
end // closing of "let t=.."
end // closing of session
"""
