open Effect;
open BsMochajs.Mocha;
open BsJsverify.Verify.Arbitrary;
open BsJsverify.Verify.Property;
open BsAbstract;
let flip = Function.flip;
let ((<.), (>.)) = Function.Infix.((<.), (>.));
let (<=<) = {
  module Fn = BsAbstract.Functions.Monad(Effect.Monad);
  Fn.compose_kliesli;
};

describe("Effect", () => {
  let pure = Effect.Applicative.pure;
  module CompareEffect = {
    type t('a) = effect('a);
    let eq = (a, b) => run_effect(a) == run_effect(b)
  };

  describe("Sanity check", () => {
    describe("Monoid in the category of endofunctors", () => {
      property1("should satisfy associativity and identity", arb_nat, x => {
        module Monoid: BsAbstract.Interface.MONOID with type t = int => effect(int) = {
          type t = int => effect(int);
          let append = (<=<);
          let empty = a => Effect.Monad.pure(a);
        };
        module Compare: BsAbstract.Interface.EQ with type t = int => effect(int) = {
          type t = int => effect(int);
          let eq = (a, b) => run_effect(a(x)) == run_effect(b(x))
        };
        module Verify_Semigroup = Verify.Compare.Semigroup(Monoid, Compare);
        module Verify_Monoid = Verify.Compare.Monoid(Monoid, Compare);
        Verify_Semigroup.associativity(pure <. (*)(3), pure <. (+)(2), pure <. (-)(1)) &&
        Verify_Monoid.identity(pure)
      });
    });
    describe("Example", () => {
      it("should flat_map correctly", () => Effect.Infix.({
        expect(
          pure(123)
            >>= (a => a == 123 ? pure(456) : pure(1))
            >>= (b => b == 456 ? pure("foo") : pure("bar"))
            <#> flip((++))("!")
          |> run_effect
        ).to_be("foo!");
      }));
    });
  });

  describe("Functor", () => {
    module V = BsAbstract.Verify.Compare.Functor(Effect.Functor, CompareEffect);
    property1("should satisfy identity", arb_nat, pure >. V.identity);
    property1("should satisfy composition", arb_nat, a => {
      V.composition((++)("!"), string_of_int, pure(a))
    })
  });
  describe("Apply", () => {
    module V = Verify.Compare.Apply(Effect.Monad, CompareEffect);
    property1(
      "should satisfy associative composition",
      arb_nat,
      pure >. V.associative_composition(pure((++)("!")), pure(string_of_int))
    )
  });
  describe("Applicative", () => {
    module V = Verify.Compare.Applicative(Effect.Applicative, CompareEffect);
    property1("should satisfy identity", arb_nat, pure >. V.identity);
    property1(
      "should satisfy homomorphism",
      arb_array(arb_nat),
      V.homomorphism(Array.Functor.map(string_of_int))
    );
    property1("should satisfy interchange", arb_nat, V.interchange(pure(string_of_int)));
  });
  describe("Monad", () => {
    module V = Verify.Compare.Monad(Effect.Monad, CompareEffect);
    property1(
      "should satisfy associativity",
      arb_nat,
      pure >. V.associativity(pure <. string_of_int, pure <. (++)("!"))
    );
    property1(
      "should satisfy identity", arb_nat, V.identity(pure <. string_of_int)
    );
  });
});
