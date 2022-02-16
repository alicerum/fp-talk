# Stuff that came to imperative languages from functional languages and made us happy.

## -1. Copyleft.

I am not going to teach you Haskell. I just want to interest you a little bit in case you haven't
ever encountered it (or other FP languages for that matter), or just share my excitement if you have.
There's plenty of hand waving going to happen today, so please try to not get frustrated and just
carry along. I'll try to keep things simple and not venture deep into details.

## 0. Some basic Haskell knowledge so we can procede

Haskell is a pure functional language. It is typed so strictly that even side effects have their
own types, but we are not going to touch that now.
Pure functions in Haskell are usually written like this:

```
inc :: Int -> Int
inc a = a + 1
```

Here, `::` is used to defite functions type. `->` is an arrow, or morphism. What it stands for,
is that this function takes an element of the set of integers and maps it to an element from the
set of integers. Here these sets are the same, and function defines this mapping. `a` is function's
argument, and `a + 1` is its return value.

In functional programming, we can usually say that function of two arguments is actually a function
of one argument that returns the function of the second argement (sometimes called a closure since
it binds an outside piece of data). Lets see a `sum` function example:

```
sum :: Int -> Int -> Int
sum a b = a + b
```

Sum takes two argements `a` and `b` and returns their addition. As we can see from the function type,
we are working with double morphism here. Actually, `->` is a right associative operator, so we can
rewrite function as follows:

```
sum :: Int -> (Int -> Int)
sum a = (\b -> a + b)
```

Here, `\b -> a + b` is a lambda function that takes one argument called b. We can call the `sum`
function like this:

```
sum 1 2
(sum 1) 2
```
These two lines mean exactly the same thing, since function application is left associative.
In the second line we only apply one argument to the function, get lambda as a return value and apply
this lambda to the second argument. Exactly same thing happens in the first line, but brackets are
removed because function application is left associative anyways. This is called currying, btw, or
partial application of functions. This is one of a very strong feature of FP, as we will see later.

I also would like to define a couple of types which we are going to deal with later:
```
data Maybe a = Nothing | Just a
data Either e a = Left e | Right a
```
`data` is used to setup a new type, `e` and `a` are polymorphic types, or type parameters. Basically it
means that we will put some real concrete type here later. We can think of them as java generics or c++
templates. `Nothing`, `Just`, `Left` and `Right` are nothing more than just type constructors. Here we
define `Maybe a` as either Nothing, or Just value. We can say that value `Just 5` has a type of `Maybe Int`.
`Right 4` has type of `Either e Int`, and `Left "error message"` has type of `Either String a`.
Maybe is usually used to return a possibly nonexistant value, while Either is usually used to return either
value or error. Value is conventionally held in the right constructor (we will see why later), while
error (be it string, or a complex structure type) in the left constructor.

## 1. Function composition

While we are used to pass variables to functions in imperative languages and compose them in such a way
that they end up containing the value we need, function composition is the way to go in functional languages.
We have already seen partial application, another way to crunch functions is function composition:

```
(.) :: (b -> c) -> (a -> b) -> a -> c
```
This function (bound to an infix operator `.`) accepts two functions and returns a new function that is a composition
of the funtions we started with, such as `(f . g) x = f (g x)`. It can be defined like this:
```
f . g = \x -> f (g x)
```
Here, composition will return a function that applies function `f` to the result of application of `g`.
Example:
```
Prelude> let f = (+3) . (*2)
Prelude> f 5
13
```

## 2. Map Reduce (without the reduce because meh)

We all have known mapreduce pattern for quite some time. It helps us process large amounts of
data in a nice concurrent way. Usually we can apply map function to elements of a collection
on order to transform them. We usually want to chain those transformations, taking elements as
far as we would possibly need them to go, and appty reduce function in the end.
This way, map induced transformations could be run concurrently, as they operate on different
amounts of non intersecting data.
This pattern is nowadays commonly used in imperative languages, i.e. Java streams:

```
int sum = widgets.stream()
	      .filter(w -> w.getColor() == RED)
	      .mapToInt(w -> w.getWeight())
	      .sum();
```

Here, `stream()` returns an instance of `Stream` interface, which is responsible for all your
maps and reduces. mapToInt converts all the elements of the collection to integers via the given
function, and sum is used to reduce. 

Apache Hadoop is a product built around this pattern. It receives big amounts of data as input,
processes it in a concurrent way and then does the necessary reduction, building all the necessary
graphs and stats.

Haskell, as any other functianal languages, obviously has a way to do map and reduce, too.
```
map :: (a -> b) -> [a] -> [b]

Prelude> map (+1) [1, 2, 3, 4]
[2,3,4,5]
```
As we can see, `map` accepts a function (in our case, partial application of (+) function to a
numeric argument `1`, and a list, and then applies function to each element of the list, thus
generating new list for all our needs.

[] is a list. Generally, map does not have to only work with lists. In haskell, we say that anything that
can be mapped upon, is a Functor. It is a class of types (you can think of class as interface or trait,
class of types is basically defined by declaration of functions that can operate on those types),
defined as follows:

```
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```
As an example, `Maybe` and `Either` are defined as functors:
```
instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just a) = Just (f a)

instance Functor (Either e) where
    fmap (Left e) = Left e
    fmap (Right a) = Right (f a)
```
The awesomeness of Maybe and Either as functors is great. We can get `Maybe` value from a function
and instead of immediate extraction of an error and possibly an early processing of error related logic,
we can rather, safely chain value modifications using `fmap` without any overhead:

```
Prelude> fmap (+1) (fmap (*3) (fmap (subtract 4) Nothing))
Nothing

Prelude> fmap (+1) (fmap (*3) (fmap (subtract 4) (Just 2)))
Just (-5)
```
Same obviously is applied to the `Either` type and many many others.

Of course, we can also use function composition as `map`'s argument, chaining even more logic into the map
application:
```
Prelude> fmap ((+3) . (*2)) (Just 3)
Just 9
```
and, since `<$>` is the operator for `map`,
```
Prelude> (+3) . (*2) <$> (Just 3)
Just 9
```
Haskell contains lots of functors (actually, every monad is by definition a functor) which we can map upon.
This is an extremely popular and useful approach that lets us chain incredibly complex operations into
function logic before feeding them the data. This is the reason why map (and reduce) has become very popular
outside functional programming, too, as soon as lambas found their way into the imperative languages.

## 3. Monads (yay) and imperative style

Lets imagine that we have an amount of operation that all return non determenistic result. Let us pretend
they all have type of `Int -> Maybe Int`, for example:

```
f1 :: Int -> Maybe Int
f1 x = if x == 0 then Nothing
       else Just (x - 1)

f2 :: Int -> Maybe Int
f2 x = if x == 1 then Nothing
       else Just (x * 2)

f3 :: Int -> Maybe Int
f3 x = if x == 4 then Nothing
       else Just (x - 7)
```

Real life computations with side effects look a lot like that, when anything can fail (files and dbs).
How do we chain them? If we try to use Maybe as a functor (because it is) and call `fmap f2 (f1 1)`,
then we are going to end up with `Just (Just 0)`. That is not what we want, as this does not allow us
to chain the third function `f3` and requires us to eventually explicitly look into the Maybe value twice.
Once for the outer Maybe, and then once for the inner (as a side note, I remember having this exact problem
in Java 8, right when lambas and Streams got into the language, solution there is using `flatMap()` method
instead of `map()` if i remember correctly).

In Haskell (and some other strictly typed FPs) we have Monads that are supposed to solve this problem (and
some others, too).
I am not going to venture into theory category now, and i'm not going to try to explain what monads are
(or what they aren't as well). Rather, let's just say that Monad is a class of types (remember, basically
an interface or trait) that has the `bind` operation defined for them (you can notice that this operation
looks very much like Haskell's logo):

```
class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
```
We can see here that `bind` operation (>>=) takes two arguments. First one is going to be our monadic value.
If we think that `Maybe` is also a monad, we will realize that `Just 5` is a monadic value of type `Maybe Int`.
Let's put `Maybe` instead of `m`:
```
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
```
And, while we are at it, we can just write Monad implementation for Maybe:
```
instance Monad Maybe where
    return a       = Just a
    Nothing  >>= _ = Nothing
    (Just a) >>= f = f a
```

Well, this just looks like a way to chain our functions now! Indeed, bind operation is defined for our Maybe
type in such a way that we can do this!

```
f4 :: Int -> Maybe Int
f4 x = f1 x  >>= (\x1 ->
       f2 x1 >>= (\x2 ->
       f3 x2 ))
```
Whoa, wait, what is happening here? `f4` takes some Int and feeds it to the `f1` function, which returns a Maybe.
Now, if we take this maybe and put it into the `bind` operator, then on the right we should provide a fuction,
that takes integer and returns another Maybe. This integer is whatever in the result of our `f1` application.
Actually, this is where `bind` operator's name comes from. Somebody very smart actually looked at it and said:
"Whoa, this look exactly like name binding. I bet we can sugar coat this!"
Indeed, we can. Ladies and gentlemen, I present to you here, an imperative code in Haskell.
```
f4 :: Int -> Maybe Int
f4 x = do
    x1 <- f1 x
    x2 <- f2 x1
    f3 x2         -- return
```
or if we want to be very very smart and functional, and use Kleisly composition,

```
f4 = f3 <=< f2 <=< f1
```
Indeed, now if any of the functions return Nothing, end result is going to be Nothing. If all functions return a
value, then we are going to have an end result. Yay.

