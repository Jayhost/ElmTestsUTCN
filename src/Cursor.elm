module Cursor exposing (Cursor(..),num,cur,count,aCurs,bCurs,cCurs,reverse,unzip,zip, back, current, forward, fromList, length, nonEmpty, toList, withSelectedElement)

reverse l =
    let
        reverseAcc lx acc =
            case lx of
                [] -> acc
                x::xs -> reverseAcc xs (x::acc)
    in
        reverseAcc l []

type Cursor a
    = Cursor (List a) a (List a)

aCurs (Cursor a b c) =
    a

bCurs (Cursor a b c) =
    b

cCurs (Cursor a b c) =
    c

withSelectedElement : List a -> a -> List a -> Cursor a
withSelectedElement left mid right =
    Cursor (List.reverse left) mid right

nonEmpty : a -> List a -> Cursor a
nonEmpty x xs =
    Cursor [] x xs


fromList : List a -> Maybe (Cursor a)
fromList l =
    case l of
    [] -> Nothing
    x::xs -> Just (withSelectedElement [] x xs)



count : List a -> Int
count l =
    let 
        cunt lis acc = 
            case lis of
            [] -> acc
            x::xs -> cunt xs (acc + 1)
    in
        cunt l 0


zip lx ly =
    case (lx, ly) of
        (x::xs, y::ys) -> (x, y) :: (zip xs ys)
        _ -> []

unzip l =
    case l of
        [] -> ([], [])
        (x,y)::ls ->
            let
                (xs, ys) = unzip ls
            in
                (x::xs, y::ys)


toList : Cursor a -> List a
toList a =
    (reverse(bCurs a::aCurs a))++cCurs a
 
current : Cursor a -> a
current (Cursor _ a _) =
    a

forward : Cursor a -> Maybe (Cursor a)
forward (Cursor a b c) =
    case c of
    [] -> Nothing
    x::xs -> Just (Cursor (b::a) x xs)



    

    
num lx ly =
    case (lx,ly) of
    (x::xs,y::ys) -> ly
    (x::xs,[])->lx
    ([],y::ys)->ly
    (_,[]) -> lx

cur a =
    aCurs a


back : Cursor a -> Maybe (Cursor a)
back (Cursor a b c) =
    case a of
    [] -> Nothing
    x::xs -> Just (Cursor xs x (b::c))


length : Cursor a -> Int
length a =
    (count (toList a))


