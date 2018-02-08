
# TODO-list

- [ ] Find out a common interface (and easier than in the current examples) 
      for tables on different css-libs.
- [ ] Find out a common interface for menus on different css-libs.
- [ ] Menu-choice: implement one that shows the current selection on widget.
- [ ] Menu-choice: implement one where multiple choices are possible.

- [ ] File-embedding: this way the js-files could be found easily with 
      jsaddle-warp exe?

## Longer term

- [ ] Add new components
- [ ] A proper I18n-mechanism
- [ ] A way to use css-libs without their associated js-libs. Implement the 
      required functionality with frp.
- [ ] Add more icon-lib examples. E.g. implement icon-lists showing icons with
      their names.
- [ ] Make webkit2gtk-versions work.
- [ ] Make android-versions work.
- [ ] Make ios-versions work.
- [ ] Start a clay-package based css-lib supporting the components we implement.
      See [clay](http://hackage.haskell.org/package/clay).

## Bootstrap

- [x] Fix the naive I18n-problem on example. The sub-widget gets initialized 
      every time the view is changed (try the menu-choice). This is similar
      to switching.
- [x] Fix the table problem on example. It initializes the table every time the
      view is changed (try the menu-choice). See the previous one (I18n).
      This maybe a bit harder than the previous one as table example tries to
      be self-contained while the I18n reads inputs from menu-selections and
      we can easily maintain state outside the widget.

## Foundation

- [ ] Add an example of naive I18n.
- [ ] Add more table examples.

## Semantic

- [ ] Add an example of naive I18n.
- [ ] Add more table examples.

## W3.css

- [ ] Add an example of naive I18n.
- [ ] Add more table examples.

