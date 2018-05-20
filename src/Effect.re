open BsAbstract.Interface;

type effect('a) = unit => 'a;

let pure: 'a => effect('a) = (a) => () => a

and flat_map: (effect('a), 'a => effect('b)) => effect('b) = (a, f) => () => f(a())();

let apply: (effect('a => 'b), effect('a)) => effect('b) = (f, a) =>
  flat_map(f, (f') => {
    flat_map(a, (a') => pure(f'(a')))
  });

let map: ('a => 'b, effect('a)) => effect('b) = (f, a) => apply(pure(f), a)

and run_effect: effect('a) => 'a = (a) => a();

module Functor: FUNCTOR with type t('a) = effect('a) = {
  type t('a) = effect('a);
  let map = map
};

module Apply: APPLY with type t('a) = effect('a) = {
  include Functor;
  let apply = apply
};

module Applicative: APPLICATIVE with type t('a) = effect('a) = {
  include Apply;
  let pure = pure
};

module Monad: MONAD with type t('a) = effect('a) = {
  include Applicative;
  let flat_map = flat_map
};

module Infix = {
  include BsAbstract.Infix.Monad(Monad)
}
