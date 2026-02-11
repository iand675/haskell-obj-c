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
  , fragmentedMovieMinderWithMovie_mindingIntervalSelector
  , initWithMovie_mindingIntervalSelector
  , addFragmentedMovieSelector
  , removeFragmentedMovieSelector
  , mindingIntervalSelector
  , setMindingIntervalSelector
  , moviesSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
    withObjCPtr movie $ \raw_movie ->
      sendClassMsg cls' (mkSelector "fragmentedMovieMinderWithMovie:mindingInterval:") (retPtr retVoid) [argPtr (castPtr raw_movie :: Ptr ()), argCDouble (fromIntegral mindingInterval)] >>= retainedObject . castPtr

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
initWithMovie_mindingInterval avFragmentedMovieMinder  movie mindingInterval =
withObjCPtr movie $ \raw_movie ->
    sendMsg avFragmentedMovieMinder (mkSelector "initWithMovie:mindingInterval:") (retPtr retVoid) [argPtr (castPtr raw_movie :: Ptr ()), argCDouble (fromIntegral mindingInterval)] >>= ownedObject . castPtr

-- | addFragmentedMovie:
--
-- Adds a fragmented movie to the array of movies being minded.
--
-- @movie@ — The fragmented movie to add to the minder.
--
-- ObjC selector: @- addFragmentedMovie:@
addFragmentedMovie :: (IsAVFragmentedMovieMinder avFragmentedMovieMinder, IsAVFragmentedMovie movie) => avFragmentedMovieMinder -> movie -> IO ()
addFragmentedMovie avFragmentedMovieMinder  movie =
withObjCPtr movie $ \raw_movie ->
    sendMsg avFragmentedMovieMinder (mkSelector "addFragmentedMovie:") retVoid [argPtr (castPtr raw_movie :: Ptr ())]

-- | removeFragmentedMovie:
--
-- Removes a fragmented movie from the array of movies being minded.
--
-- @movie@ — The fragmented movie to remove from the minder.
--
-- ObjC selector: @- removeFragmentedMovie:@
removeFragmentedMovie :: (IsAVFragmentedMovieMinder avFragmentedMovieMinder, IsAVFragmentedMovie movie) => avFragmentedMovieMinder -> movie -> IO ()
removeFragmentedMovie avFragmentedMovieMinder  movie =
withObjCPtr movie $ \raw_movie ->
    sendMsg avFragmentedMovieMinder (mkSelector "removeFragmentedMovie:") retVoid [argPtr (castPtr raw_movie :: Ptr ())]

-- | mindingInterval
--
-- An NSTimeInterval indicating how often a check for additional movie fragments should be performed. The default interval is 10.0.
--
-- ObjC selector: @- mindingInterval@
mindingInterval :: IsAVFragmentedMovieMinder avFragmentedMovieMinder => avFragmentedMovieMinder -> IO CDouble
mindingInterval avFragmentedMovieMinder  =
  sendMsg avFragmentedMovieMinder (mkSelector "mindingInterval") retCDouble []

-- | mindingInterval
--
-- An NSTimeInterval indicating how often a check for additional movie fragments should be performed. The default interval is 10.0.
--
-- ObjC selector: @- setMindingInterval:@
setMindingInterval :: IsAVFragmentedMovieMinder avFragmentedMovieMinder => avFragmentedMovieMinder -> CDouble -> IO ()
setMindingInterval avFragmentedMovieMinder  value =
  sendMsg avFragmentedMovieMinder (mkSelector "setMindingInterval:") retVoid [argCDouble (fromIntegral value)]

-- | movies
--
-- An NSArray of the AVFragmentedMovie objects being minded.
--
-- ObjC selector: @- movies@
movies :: IsAVFragmentedMovieMinder avFragmentedMovieMinder => avFragmentedMovieMinder -> IO (Id NSArray)
movies avFragmentedMovieMinder  =
  sendMsg avFragmentedMovieMinder (mkSelector "movies") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fragmentedMovieMinderWithMovie:mindingInterval:@
fragmentedMovieMinderWithMovie_mindingIntervalSelector :: Selector
fragmentedMovieMinderWithMovie_mindingIntervalSelector = mkSelector "fragmentedMovieMinderWithMovie:mindingInterval:"

-- | @Selector@ for @initWithMovie:mindingInterval:@
initWithMovie_mindingIntervalSelector :: Selector
initWithMovie_mindingIntervalSelector = mkSelector "initWithMovie:mindingInterval:"

-- | @Selector@ for @addFragmentedMovie:@
addFragmentedMovieSelector :: Selector
addFragmentedMovieSelector = mkSelector "addFragmentedMovie:"

-- | @Selector@ for @removeFragmentedMovie:@
removeFragmentedMovieSelector :: Selector
removeFragmentedMovieSelector = mkSelector "removeFragmentedMovie:"

-- | @Selector@ for @mindingInterval@
mindingIntervalSelector :: Selector
mindingIntervalSelector = mkSelector "mindingInterval"

-- | @Selector@ for @setMindingInterval:@
setMindingIntervalSelector :: Selector
setMindingIntervalSelector = mkSelector "setMindingInterval:"

-- | @Selector@ for @movies@
moviesSelector :: Selector
moviesSelector = mkSelector "movies"

