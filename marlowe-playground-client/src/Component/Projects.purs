module Component.Projects where

import Prelude

import Component.Projects.Open as Open
import Component.Projects.Save as Save
import Halogen.HTML as HH
import Type.Prelude (Proxy(..))

save = Save.component

open = Open.component
