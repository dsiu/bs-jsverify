open RescriptMocha.Mocha
open Verify.Arbitrary
open Verify.Property

describe("JsVerify", () => {
  property1("boolean `not` (involution)", arb_bool, b => !(!b) == b)

  property3("nat `+` (associative)", arb_nat, arb_nat, arb_nat, (n1, n2, n3) =>
    n1 + n2 + n3 == n1 + (n2 + n3)
  )

  property1("Js.null(nat)", arb_null(arb_nat), n =>
    switch Null.toOption(n) {
    | Some(n') => n' >= 0
    | None => true
    }
  )

  property1("Js.nullable(nat)", arb_nullable(arb_nat), n =>
    switch Nullable.toOption(n) {
    | Some(n') => n' >= 0
    | None => true
    }
  )

  property1("option(nat)", arb_option(arb_nat), n =>
    switch n {
    | Some(n') => n' >= 0
    | None => true
    }
  )

  property1("sum of nats is >= 0", arb_array(arb_nat), a => a->Array.reduce(0, \"+") >= 0)

  let arb_record_c = unsafe_arb_record(
    (Proxy: Types.proxy<{"d": bool, "e": Null.t<int>}>),
    {"d": arb_js_bool, "e": arb_null(arb_nat)},
  )

  property1(
    "unsafe_arb_record",
    unsafe_arb_record(
      (Proxy: Types.proxy<{"a": string, "b": int, "c": {"d": bool, "e": Null.t<int>}}>),
      {"a": arb_string, "b": arb_nat, "c": arb_record_c},
    ),
    r =>
      switch Type.Classify.classify(r["a"]) {
      | String(_) => true
      | _ => false
      } &&
      (switch Type.Classify.classify(r["b"]) {
      | Number(n) => n >= 0.0
      | _ => false
      } &&
      switch Type.Classify.classify(r["c"]) {
      | Object(o) =>
        switch Type.Classify.classify(Obj.magic(o)["d"]) {
        | Bool(_) => true
        | _ => false
        } &&
        switch Type.Classify.classify(Obj.magic(o)["e"]) {
        | Null => true
        | Number(n) => n >= 0.0
        | _ => false
        }
      | _ => false
      }),
  )

  property1("testing tuple", arb_tuple((arb_nat, arb_nat)), ((a, b)) => a + b >= a && a + b >= b)

  property1("testing sum", arb_sum((arb_nat, arb_string)), s =>
    switch Type.Classify.classify(s) {
    | String(_)
    | Number(_) => true
    | _ => false
    }
  )

  property1("testing either", arb_either(arb_nat, arb_string), e =>
    switch e {
    | Left(l) =>
      switch Type.Classify.classify(l) {
      | Number(n) => n >= 0.0
      | _ => false
      }
    | Right(r) =>
      switch Type.Classify.classify(r) {
      | String(_) => true
      | _ => false
      }
    }
  )
})
