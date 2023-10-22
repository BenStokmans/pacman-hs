module FontContainer where

import SDL.Font (Font, load)

data FontContainer = FontContainer
    {
        xxl :: Font, -- 128px
        xl  :: Font, -- 64px
        l   :: Font, -- 32px
        m   :: Font, -- 16px
        s   :: Font, -- 8px
        xs  :: Font -- 4px
    }

loadFont :: String -> IO FontContainer
loadFont p = do
    xxl <- load p 128
    xl <- load p 64
    l <- load p 32
    m <- load p 16
    s <- load p 8
    xs <- load p 4
    return FontContainer
        {
            xxl = xxl,
            xl = xl,
            l = l,
            m = m,
            s = s,
            xs = xs
        }
