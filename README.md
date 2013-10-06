FastAGI
=======

Haskell Framework for creating FastAGI Servers

## Work in Progress

This framework still needs some work. If you want to help, then implement any function listed [here](http://www.voip-info.org/wiki/view/Asterisk%20AGI). 

## Example

```haskell
example =
  startServer $
  do answer
     digit <- waitForDigit Nothing
     case digit of
     	Just (AGISuccess One) ->
     		liftIO $ putStrLn ("Someone pressed 1!")
     	_ ->
     		liftIO $ putStrLn ("Other number")
     hangup Nothing
```