### Hedwig-Styled

A library for Hedwig that will allow you to apply inline-style to elements, including pseudo classes.

Here's an example of how to define some styles in hedwig-styled:

```
view :: Model -> H.Html Msg
view model = toUnstyled $
 div [Style Nothing "margin" "auto"] []
   [ button
     [ Style Nothing "color" "red"
     , Style (Just Hover) "color" "blue"
     ]
     []
     [ StyledText "helllloooo Harry!" ]
   ]

```
#### To run the tests

Run the commands
```
npm i
bower install
pulp test

```
