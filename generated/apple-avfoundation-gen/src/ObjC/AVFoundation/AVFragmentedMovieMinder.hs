{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVFragmentedMovieMinder
--
-- A class that periodically checks whether additional movie fragments have been appended to fragmented movie files.
--
-- AVFragmentedMovieMinder is identical to AVFragmentedAssetMinder except that it's capable of minding only assets of class AVFragmentedMovie.
--
-- Generated bindings for @AVFragmentedMovieMinder@.
module ObjC.AVFoundation.AVFragmentedMovieMinder
  ( AVFragmentedMovieMinder
  , IsAVFragmentedMovieMinder(..)
  , fragmentedMovieMinderWithMovie_mindingInterval
  , initWithMovie_mindingInterval
  , addFragmentedMovie
  , removeFragmentedMovie
  , mindingInterval
  , setMindingInterval
  , movies
  , addFragmentedMovieSelector
  , fragmentedMovieMinderWithMovie_mindingIntervalSelector
  , initWithMovie_mindingIntervalSelector
  , mindingIntervalSelector
  , moviesSelector
  , removeFragmentedMovieSelector
  , setMindingIntervalSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | fragmentedMovieMinderWithMovie:mindingInterval:
--
-- Creates an AVFragmentedMovieMinder, adds the specified movie to it, and sets the mindingInterval to the specified value.
--
-- @movie@ — An instance of AVFragmentedMovie to add to the AVFragmentedMovieMinder
--
-- @mindingInterval@ — The initial minding interval of the AVFragmentedMovieMinder.
--
-- Returns: A new instance of AVFragmentedMovieMinder.
--
-- ObjC selector: @+ fragmentedMovieMinderWithMovie:mindingInterval:@
fragmentedMovieMinderWithMovie_mindingInterval :: IsAVFragmentedMovie movie => movie -> CDouble -> IO (Id AVFragmentedMovieMinder)
fragmentedMovieMinderWithMovie_mindingInterval movie mindingInterval =
  do
    cls' <- getRequiredClass "AVFragmentedMovieMinder"
    sendClassMessage cls' fragmentedMovieMinderWithMovie_mindingIntervalSelector (toAVFragmentedMovie movie) mindingInterval

-- | initWithMovie:mindingInterval:
--
-- Creates an AVFragmentedMovieMinder, adds the specified movie to it, and sets the mindingInterval to the specified value.
--
-- @movie@ — An instance of AVFragmentedMovie to add to the AVFragmentedMovieMinder
--
-- @mindingInterval@ — The initial minding interval of the AVFragmentedMovieMinder.
--
-- Returns: A new instance of AVFragmentedMovieMinder.
--
-- ObjC selector: @- initWithMovie:mindingInterval:@
initWithMovie_mindingInterval :: (IsAVFragmentedMovieMinder avFragmentedMovieMinder, IsAVFragmentedMovie movie) => avFragmentedMovieMinder -> movie -> CDouble -> IO (Id AVFragmentedMovieMinder)
initWithMovie_mindingInterval avFragmentedMovieMinder movie mindingInterval =
  sendOwnedMessage avFragmentedMovieMinder initWithMovie_mindingIntervalSelector (toAVFragmentedMovie movie) mindingInterval

-- | addFragmentedMovie:
--
-- Adds a fragmented movie to the array of movies being minded.
--
-- @movie@ — The fragmented movie to add to the minder.
--
-- ObjC selector: @- addFragmentedMovie:@
addFragmentedMovie :: (IsAVFragmentedMovieMinder avFragmentedMovieMinder, IsAVFragmentedMovie movie) => avFragmentedMovieMinder -> movie -> IO ()
addFragmentedMovie avFragmentedMovieMinder movie =
  sendMessage avFragmentedMovieMinder addFragmentedMovieSelector (toAVFragmentedMovie movie)

-- | removeFragmentedMovie:
--
-- Removes a fragmented movie from the array of movies being minded.
--
-- @movie@ — The fragmented movie to remove from the minder.
--
-- ObjC selector: @- removeFragmentedMovie:@
removeFragmentedMovie :: (IsAVFragmentedMovieMinder avFragmentedMovieMinder, IsAVFragmentedMovie movie) => avFragmentedMovieMinder -> movie -> IO ()
removeFragmentedMovie avFragmentedMovieMinder movie =
  sendMessage avFragmentedMovieMinder removeFragmentedMovieSelector (toAVFragmentedMovie movie)

-- | mindingInterval
--
-- An NSTimeInterval indicating how often a check for additional movie fragments should be performed. The default interval is 10.0.
--
-- ObjC selector: @- mindingInterval@
mindingInterval :: IsAVFragmentedMovieMinder avFragmentedMovieMinder => avFragmentedMovieMinder -> IO CDouble
mindingInterval avFragmentedMovieMinder =
  sendMessage avFragmentedMovieMinder mindingIntervalSelector

-- | mindingInterval
--
-- An NSTimeInterval indicating how often a check for additional movie fragments should be performed. The default interval is 10.0.
--
-- ObjC selector: @- setMindingInterval:@
setMindingInterval :: IsAVFragmentedMovieMinder avFragmentedMovieMinder => avFragmentedMovieMinder -> CDouble -> IO ()
setMindingInterval avFragmentedMovieMinder value =
  sendMessage avFragmentedMovieMinder setMindingIntervalSelector value

-- | movies
--
-- An NSArray of the AVFragmentedMovie objects being minded.
--
-- ObjC selector: @- movies@
movies :: IsAVFragmentedMovieMinder avFragmentedMovieMinder => avFragmentedMovieMinder -> IO (Id NSArray)
movies avFragmentedMovieMinder =
  sendMessage avFragmentedMovieMinder moviesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fragmentedMovieMinderWithMovie:mindingInterval:@
fragmentedMovieMinderWithMovie_mindingIntervalSelector :: Selector '[Id AVFragmentedMovie, CDouble] (Id AVFragmentedMovieMinder)
fragmentedMovieMinderWithMovie_mindingIntervalSelector = mkSelector "fragmentedMovieMinderWithMovie:mindingInterval:"

-- | @Selector@ for @initWithMovie:mindingInterval:@
initWithMovie_mindingIntervalSelector :: Selector '[Id AVFragmentedMovie, CDouble] (Id AVFragmentedMovieMinder)
initWithMovie_mindingIntervalSelector = mkSelector "initWithMovie:mindingInterval:"

-- | @Selector@ for @addFragmentedMovie:@
addFragmentedMovieSelector :: Selector '[Id AVFragmentedMovie] ()
addFragmentedMovieSelector = mkSelector "addFragmentedMovie:"

-- | @Selector@ for @removeFragmentedMovie:@
removeFragmentedMovieSelector :: Selector '[Id AVFragmentedMovie] ()
removeFragmentedMovieSelector = mkSelector "removeFragmentedMovie:"

-- | @Selector@ for @mindingInterval@
mindingIntervalSelector :: Selector '[] CDouble
mindingIntervalSelector = mkSelector "mindingInterval"

-- | @Selector@ for @setMindingInterval:@
setMindingIntervalSelector :: Selector '[CDouble] ()
setMindingIntervalSelector = mkSelector "setMindingInterval:"

-- | @Selector@ for @movies@
moviesSelector :: Selector '[] (Id NSArray)
moviesSelector = mkSelector "movies"

