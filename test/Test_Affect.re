open Affect;
open BsMochajs.Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
open BsAbstract;
module Fn = BsAbstract.Functions.Monad(Affect.Monad);
let (flip, const) = Function.(flip, const);
let ((<.), (>.)) = Function.Infix.((<.), (>.));
let (<=<) = Fn.compose_kliesli;

module CompareAffect = {
  let (>>=) = Affect.Infix.(>>=);
  type t('a) = affect('a);
  let eq = (a, b) => run_affect(a >>= (_) => pure()) == run_affect(b >>= (_) => pure())
};

let dirname : option(string) = [%bs.node __dirname];
let dirname' = Js.Option.getWithDefault(".", dirname);

let read_file : string => affect(string) = path => (error, success) =>
  Fs.read_file_async(path, `utf8, (err, content) => {
    err != Js.null ? error(Js.Null.to_opt(err)) : success(content);
  });

let read_file' : string => Js.Promise.t(string) = path =>
  Js.Promise.make((~resolve, ~reject) => {
    Fs.read_file_async(path, `utf8, (err, content) => {
      switch (Js.Null.to_opt(err), content) {
        | (Some(err'), _) => [@bs] reject(BsErrors.Error.unsafe_to_exception(err'))
        | (_, content) => [@bs] resolve(content)
      }
    });
  });

let write_file : (string, string) => affect(unit) = (path, content) => (error, success) =>
  Fs.write_file_async(path, content, `utf8, err => {
    err != Js.null ? error(Js.Null.to_opt(err)) : success();
  });

let delay : int => affect(unit) = ms => (_, success) =>
  Js.Global.setTimeout(() => success(), ms) |> ignore;


describe("Affect", () => Affect.Infix.({
  before(() => {
    Fs.write_file_sync("sample", "hello", `utf8);
  });

  describe("Sanity check", () => {
    describe("Monoid in the category of endofunctors", () => {
      property1("should satisfy associativity and identity", arb_nat, x => {
        module Monoid: BsAbstract.Interface.MONOID with type t = int => affect(int) = {
          type t = int => affect(int);
          let append = (<=<);
          let empty = a => Affect.Monad.pure(a);
        };
        module Compare: BsAbstract.Interface.EQ with type t = int => affect(int) = {
          type t = int => affect(int);
          let eq = (a, b) =>
            run_affect(a(x) >>= (_) => pure()) == run_affect(b(x) >>= (_) => pure())
        };
        module Verify_Semigroup = Verify.Compare.Semigroup(Monoid, Compare);
        module Verify_Monoid = Verify.Compare.Monoid(Monoid, Compare);
        Verify_Semigroup.associativity(pure <. (*)(3), pure <. (+)(2), pure <. (-)(1)) &&
        Verify_Monoid.identity(pure)
      });
    });
    describe("Chaining effects", () => {
      it'("should chain the async effects correctly", done_ => {
        read_file("sample")
          >>= (content => write_file("sample", content ++ " world!"))
          >>= ((_) => read_file("sample"))
          >>= (content => {
                expect(content).to_be("hello world!");
                done_() |> pure
              })
          |> run_affect;
      });
    });

    describe("Throwing effects", () => {
      it'("should not throw if the async effect didn't fail", done_ => {
        try ({
          Affect.throw(read_file("sample")) |> ignore;
          /* Successfully didn't throw */
          done_();
        }) { | Js.Exn.Error(_) => should_be_ok(Js.false_) }
      });

      it'("should throw if the async effect failed", done_ => {
        try ({
          Affect.throw(read_file("non-existent-name")) |> ignore;
          /* Did not throw */
          should_be_ok(Js.false_);
          done_();
        }) { | Js.Exn.Error(_) => done_() }
      });
    });

    describe("Parallel effects", () => {
      it'("should run effects in parallel (parallel)", done_ => {
        let x = ref("") and y = ref(0);
        let delay_ms = 500;
        let delay_x = delay(delay_ms) >>= () => { x := "foo"; pure(x^) }
        and delay_y = delay(delay_ms) >>= () => { y := 123; pure(y^) };

        parallel(delay_x, delay_y) >>= (() => pure()) |> run_affect;

        Js.Global.setTimeout(() => {
          expect(x^).to_be("foo");
          expect(y^).to_be(123);
          done_()
        }, delay_ms) |> ignore;
      });
      it'("should run effects in parallel (parallel')", done_ => {
        let x = ref("") and y = ref("");
        let delay_ms = 500;
        let delay_x = delay(delay_ms) >>= () => { x := "foo"; pure() }
        and delay_y = delay(delay_ms) >>= () => { y := "bar"; pure() };

        parallel'([delay_x, delay_y]) |> run_affect;

        Js.Global.setTimeout(() => {
          expect(x^).to_be("foo");
          expect(y^).to_be("bar");
          done_()
        }, delay_ms) |> ignore;
      });
    })
  });
  describe("Functor", () => {
    module V = BsAbstract.Verify.Compare.Functor(Affect.Functor, CompareAffect);
    property1("should satisfy identity", arb_nat, pure >. V.identity);
    property1("should satisfy composition", arb_nat, a => {
      V.composition((++)("!"), string_of_int, pure(a))
    })
  });
  describe("Apply", () => {
    module V = Verify.Compare.Apply(Affect.Monad, CompareAffect);
    property1(
      "should satisfy associative composition",
      arb_nat,
      pure >. V.associative_composition(pure((++)("!")), pure(string_of_int))
    )
  });
  describe("Applicative", () => {
    module V = Verify.Compare.Applicative(Affect.Applicative, CompareAffect);
    property1("should satisfy identity", arb_nat, pure >. V.identity);
    property1(
      "should satisfy homomorphism",
      arb_array(arb_nat),
      V.homomorphism(Array.Functor.map(string_of_int))
    );
    property1("should satisfy interchange", arb_nat, V.interchange(pure(string_of_int)));
  });
  describe("Monad", () => {
    module V = Verify.Compare.Monad(Affect.Monad, CompareAffect);
    property1(
      "should satisfy associativity",
      arb_nat,
      pure >. V.associativity(pure <. string_of_int, pure <. (++)("!"))
    );
    property1(
      "should satisfy identity", arb_nat, V.identity(pure <. string_of_int)
    );
  });
  describe("to_promise", () => {
    it'("should convert to a promise", done_ => {
      read_file("sample")
        |> to_promise
        |> Js.Promise.then_(result => {
             expect(result).to_be("hello world!");
             Js.Promise.resolve(done_())
           })
        |> ignore
    })
  });
  describe("from_promise", () => {
    it'("should convert from a promise", done_ => {
      read_file'("sample")
        |> from_promise
        >>= (result => {
             expect(result).to_be("hello world!");
             pure(done_())
           })
        |> run_affect
    })
  });
}));
