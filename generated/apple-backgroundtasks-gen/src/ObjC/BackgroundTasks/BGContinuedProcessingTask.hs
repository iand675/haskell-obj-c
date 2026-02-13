{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A task meant to perform processing on behalf of a user initiated request.
--
-- Continued processing tasks will present UI while in progress to provide awareness to the user. ``BGContinuedProcessingTask``s _must_ report progress via the ``NSProgressReporting`` protocol conformance during runtime and are subject to expiration based on changing system conditions and user input. Tasks that appear stalled may be forcibly expired by the scheduler to preserve system resources.
--
-- Generated bindings for @BGContinuedProcessingTask@.
module ObjC.BackgroundTasks.BGContinuedProcessingTask
  ( BGContinuedProcessingTask
  , IsBGContinuedProcessingTask(..)
  , updateTitle_subtitle
  , title
  , subtitle
  , subtitleSelector
  , titleSelector
  , updateTitle_subtitleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BackgroundTasks.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Update the title and subtitle displayed in the live activity displayed to the user.
--
-- - Parameters:   - title: The localized title displayed to the user.   - subtitle: The localized subtitle displayed to the user.
--
-- ObjC selector: @- updateTitle:subtitle:@
updateTitle_subtitle :: (IsBGContinuedProcessingTask bgContinuedProcessingTask, IsNSString title, IsNSString subtitle) => bgContinuedProcessingTask -> title -> subtitle -> IO ()
updateTitle_subtitle bgContinuedProcessingTask title subtitle =
  sendMessage bgContinuedProcessingTask updateTitle_subtitleSelector (toNSString title) (toNSString subtitle)

-- | The localized title displayed to the user.
--
-- ObjC selector: @- title@
title :: IsBGContinuedProcessingTask bgContinuedProcessingTask => bgContinuedProcessingTask -> IO (Id NSString)
title bgContinuedProcessingTask =
  sendMessage bgContinuedProcessingTask titleSelector

-- | The localized subtitle displayed to the user.
--
-- ObjC selector: @- subtitle@
subtitle :: IsBGContinuedProcessingTask bgContinuedProcessingTask => bgContinuedProcessingTask -> IO (Id NSString)
subtitle bgContinuedProcessingTask =
  sendMessage bgContinuedProcessingTask subtitleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateTitle:subtitle:@
updateTitle_subtitleSelector :: Selector '[Id NSString, Id NSString] ()
updateTitle_subtitleSelector = mkSelector "updateTitle:subtitle:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector '[] (Id NSString)
subtitleSelector = mkSelector "subtitle"

