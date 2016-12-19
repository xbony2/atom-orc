{- Dining Philosophers -}

{- Randomly swap order of fork pick-up -}
def shuffle(a,b) = if (Random(2) = 1) then (a,b) else (b,a)

def take((a,b)) =
  a.acquire() >> b.acquireD() ;
  a.release() >> take(shuffle(a,b))

def drop(a,b) = (a.release(), b.release()) >> signal

{- Define a philosopher -}
def phil(n,a,b) =
  def thinking() =
    Println(n + " thinking") >>
    (if (Random(10) <: 9)
      then Rwait(Random(1000))
      else stop)
  def hungry() = take((a,b))
  def eating() =
    Println(n + " eating") >>
    Rwait(Random(1000)) >>
    Println(n + " done eating") >>
    drop(a,b)
  thinking() >> hungry() >> eating() >> phil(n,a,b)

def philosophers(1,a,b) = phil(1,a,b)
def philosophers(n,a,b) =
  val c = Semaphore(1)
  philosophers(n-1,a,c) | phil(n,c,b)

{- Test the definitions -}
val fork = Semaphore(1)
philosophers(5,fork,fork)

{-
OUTPUT:EXAMPLE
5 thinking
4 thinking
3 thinking
2 thinking
1 thinking
1 eating
4 eating
...
-}
