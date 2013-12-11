module Graphics.WebGL (link, bind, encapsulate, webgl) where

{-| WebGL -}

import Graphics.Element (Element)
import Native.Graphics.WebGL
import Signal (Signal)

data Shader a u v = Dummy_Shader
data Program a u = Dummy_Program

link : Shader a u v -> Shader {} {} v -> Program a u
link = Native.Graphics.WebGL.link

data Buffer a = Dummy_Buffer

bind : [a] -> Buffer a
bind = Native.Graphics.WebGL.bind

data Model = Dummy_Model 

encapsulate : Program a u -> Buffer a -> u -> Model
encapsulate = Native.Graphics.WebGL.encapsulate

webgl : (Int,Int) -> [Model] -> Element
webgl = Native.Graphics.WebGL.webgl
