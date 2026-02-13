{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMovie@.
module ObjC.AppKit.NSMovie
  ( NSMovie
  , IsNSMovie(..)
  , initWithCoder
  , init_
  , initWithMovie
  , qtMovie
  , initSelector
  , initWithCoderSelector
  , initWithMovieSelector
  , qtMovieSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCoder:@
initWithCoder :: (IsNSMovie nsMovie, IsNSCoder coder) => nsMovie -> coder -> IO (Id NSMovie)
initWithCoder nsMovie coder =
  sendOwnedMessage nsMovie initWithCoderSelector (toNSCoder coder)

-- | @- init@
init_ :: IsNSMovie nsMovie => nsMovie -> IO (Id NSMovie)
init_ nsMovie =
  sendOwnedMessage nsMovie initSelector

-- | @- initWithMovie:@
initWithMovie :: IsNSMovie nsMovie => nsMovie -> RawId -> IO (Id NSMovie)
initWithMovie nsMovie movie =
  sendOwnedMessage nsMovie initWithMovieSelector movie

-- | @- QTMovie@
qtMovie :: IsNSMovie nsMovie => nsMovie -> IO RawId
qtMovie nsMovie =
  sendMessage nsMovie qtMovieSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSMovie)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSMovie)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMovie:@
initWithMovieSelector :: Selector '[RawId] (Id NSMovie)
initWithMovieSelector = mkSelector "initWithMovie:"

-- | @Selector@ for @QTMovie@
qtMovieSelector :: Selector '[] RawId
qtMovieSelector = mkSelector "QTMovie"

