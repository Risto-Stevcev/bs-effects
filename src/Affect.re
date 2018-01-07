open BsAbstract.Interface;

type affect('a) = (option(Js.Exn.t) => unit, 'a => unit) => unit;

let pure: 'a => affect('a) = a => (_, success) => success(a);

let flat_map: (affect('a), 'a => affect('b)) => affect('b) = (callback, f) =>
  (error, success) =>
    callback(error, x => {
      let callback' = f(x);
      callback'(error, success);
    });

let apply: (affect('a => 'b), affect('a)) => affect('b) = (f, a) =>
  flat_map(f, (f') => {
    flat_map(a, (a') => pure(f'(a')));
  });

let map: ('a => 'b, affect('a)) => affect('b) = (f, a) => apply(pure(f), a);

let run_affect: affect(unit) => unit = callback => callback((_) => (), () => ());

let to_effect: affect(unit) => Effect.effect(unit) =
  callback => callback((_) => (), () => ()) |> Effect.pure;

let throw: affect('a) => affect(unit) = callback =>
  (_, _) =>
    callback(
      error => switch (BsAbstract.Option.Monad.flat_map(error, Js.Exn.message)) {
        | Some(message) => Js.Exn.raiseError(message)
        | _ => ()
        },
      (_) => ()
    );

let parallel: (affect('a), affect('b)) => affect(unit) = (aff_a, aff_b) =>
  (error, success) => {
    flat_map(aff_a, (_) => pure())(error, success);
    flat_map(aff_b, (_) => pure())(error, success);
  };

let parallel': list(affect('a)) => affect(unit) = affects =>
  (error, success) => List.iter(affect => affect(error, success), affects);

[@bs.send.pipe: Js.Promise.t('a)] external promise_then_ :
  ('a => Js.Promise.t('b), Js.Exn.t => Js.Promise.t('b)) => Js.Promise.t('b) = "then";

let to_promise: affect('a) => Js.Promise.t('a) = callback =>
  Js.Promise.make((~resolve, ~reject) => {
    callback(
      error => switch error {
      | Some(error') => [@bs] reject(BsErrors.Error.unsafe_to_exception(error'))
      | None => ()
      },
      success => [@bs] resolve(success)
    )
  });

let from_promise: Js.Promise.t('a) => affect('a) = promise =>
  (error, success) => (
    promise_then_(
      success' => {
        success(success');
        promise;
      },
      error' => {
        error(Some(error'));
        promise;
      },
      promise
    ) |> ignore
  );

module Functor: FUNCTOR with type t('a) = affect('a) = {
  type t('a) = affect('a);
  let map = map;
};
module Apply: APPLY with type t('a) = affect('a) = {
  include Functor;
  let apply = apply;
};
module Applicative: APPLICATIVE with type t('a) = affect('a) = {
  include Apply;
  let pure = pure;
};
module Monad: MONAD with type t('a) = affect('a) = {
  include Applicative;
  let flat_map = flat_map;
};
module Infix = {
  include BsAbstract.Infix.Monad(Monad);
};
