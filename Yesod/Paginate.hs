{-# LANGUAGE QuasiQuotes, TemplateHaskell, CPP, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
-- | Provides a pagination subsite for Yesod.
--
-- Route layout:
--
-- > /           PaginateHomeR   GET
-- > /#Int       PaginateStartR  GET
-- > /#Int/#Int  PaginateR       GET
--
-- If no numbers are given, display the items starting from the beginning with the default count. If one number is given,
-- display the items starting from that index with the default count. If two numbers are given, the first one is the count
-- and the second is the starting index. (This is so the user can modify the end of the URL to move forward in the pagination.)
module Yesod.Paginate
  ( Paginate (..)
  , PaginateRoute (..)
  , defaultPaginate
  , prevLink
  , nextLink
  )
  where

import Yesod
import Control.Applicative
import Language.Haskell.TH.Syntax

data Paginate master rep a = Paginate
  { pgnDefaultCount :: Int                                                              -- ^ How many items to show per page by default
  , pgnGetItems     :: Int -> Int -> GHandler (Paginate master rep a) master [a]        -- ^ Get a certain count of items at a certain offset
  , pgnItemCount    :: GHandler (Paginate master rep a) master Int                      -- ^ How many items there are in all
  , pgnDisplayItems :: Int -> Int -> [a] -> GHandler (Paginate master rep a) master rep -- ^ Render the items on a page given the count and offset
  }

mkYesodSub
  "Paginate master rep a"
  [ ClassP ''Yesod [VarT $ mkName "master"]
  , ClassP ''HasReps [VarT $ mkName "rep"]
  ]
#if GHC7
  [parseRoutes|
#else
  [$parseRoutes|
#endif
/           PaginateHomeR   GET
/#Int       PaginateStartR  GET
/#Int/#Int  PaginateR       GET
|]

getPaginateHomeR :: GHandler (Paginate master rep a) master rep
getPaginateHomeR = do
  pgn <- getYesodSub
  toMaster <- getRouteToMaster
  redirect RedirectSeeOther (toMaster (PaginateR (pgnDefaultCount pgn) 0))

getPaginateStartR :: Int -> GHandler (Paginate master rep a) master rep
getPaginateStartR start = do
  pgn <- getYesodSub
  toMaster <- getRouteToMaster
  redirect RedirectSeeOther (toMaster (PaginateR (pgnDefaultCount pgn) start))

getPaginateR :: Int -> Int -> GHandler (Paginate master rep a) master rep
getPaginateR howmany start = do
  pgn <- getYesodSub
  xs <- pgnGetItems pgn howmany start
  pgnDisplayItems pgn howmany start xs

defaultPaginate
  :: (YesodPersist master, PersistBackend (YesodDB master (GHandler (Paginate master rep a) master)), PersistEntity a)
  => Int                                                                -- ^ Default number of items to show
  -> [Filter a]                                                         -- ^ Filters to apply
  -> [Order a]                                                          -- ^ Ordering to apply
  -> (Int -> Int -> [a] -> GHandler (Paginate master rep a) master rep)     -- ^ Display function
  -> Paginate master rep a
defaultPaginate x fs os d = Paginate
  { pgnDefaultCount = x
  , pgnGetItems = \y z -> map snd <$> runDB (selectList fs os y z)
  , pgnItemCount = runDB (count fs)
  , pgnDisplayItems = d
  }

-- | Link to the previous page.
prevLink :: Paginate master rep a -> Int -> Int -> Maybe (Route (Paginate master rep a))
prevLink p howmany start
  | start > 0 = Just (PaginateR howmany (max 0 (start-howmany)))
  | otherwise = Nothing

-- | Link to the next page.
nextLink :: Paginate master rep a -> Int -> Int -> GHandler (Paginate master rep a) master (Maybe (Route (Paginate master rep a)))
nextLink p howmany start = go <$> pgnItemCount p where
  go l | start < l-howmany-1 = Just (PaginateR howmany (start+howmany))
       | otherwise           = Nothing
