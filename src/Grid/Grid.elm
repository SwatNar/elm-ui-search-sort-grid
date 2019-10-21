module Grid.Grid exposing
    ( Column
    , ColumnDefinition
    , ColumnDefinitions
    , ColumnSettings(..)
    , ColumnToggle
    , ColumnToggles
    , Getter(..)
    , GridModel
    , Model
    , Msg(..)
    , SortBy(..)
    , Sorter
    , columnTogglesView
    , defaultGridModel
    , filterView
    , gridView
    , update
    )

import Browser.Dom as Dom
import Element as El exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import PAM.UI.Basic as UI exposing (Edges, Openness(..), PanelDimensions, WindowSize, borders, corners, edges)
import PAM.UI.Select as Select exposing (..)
import Task
import Process


--
-- MODEL
--


type alias GridModel =
    Model


type alias Model =
    { itemsPerPage : Int
    , currentPage : Int
    , search : Maybe String
    , sort : SortBy
    , filterColumn : Maybe ColumnFilter
    , filterDropdownOpenness : Openness
    , columnSettingsOpenness : Openness
    , itemsPerPageOpenness : Openness
    }


defaultGridModel : GridModel
defaultGridModel =
    { itemsPerPage = 10
    , currentPage = 0
    , search = Nothing
    , sort = Sort_None
    , filterColumn = Nothing
    , filterDropdownOpenness = Closed
    , columnSettingsOpenness = Closed
    , itemsPerPageOpenness = Closed
    }



--
-- SUPPORTING TYPES
--


type alias ColumnToggles msg =
    List (ColumnToggle msg)


type alias ColumnToggle msg =
    { typeId : Int
    , modelId : Int
    , title : String
    , shown : Bool
    , toggleMessage : msg
    }


type alias ColumnFilter =
    { title : String
    }


type alias ColumnDefinitions a msg =
    List (ColumnDefinition a msg)


type alias ColumnDefinition a msg =
    Column a msg


type alias Column a msg =
    { typeId : Int
    , modelId : Maybe Int
    , getter : Getter a msg
    , title : String
    , sort : Sorter a
    , settings : ColumnSettings
    , shown : Bool
    }


type alias Sorter a =
    { order : List a -> List a
    }


type SortBy
    = Sort_None
    | Sort String
    | Sort_Reverse String


type ColumnSettings
    = AllowHideShow
    | DisableHideShow


type Getter a msg
    = StringGetter (a -> String)
    | IntGetter (a -> Int)
    | FloatGetter (a -> Float)
    | DynamicGetter (a -> ( Element msg, String ))



--
-- MSG & UPDATE
--


type Msg a msg
    = NoOp
    | SortBy String
    | UpdatePage Int
    | GotoPage Int
    | PageSizeChanged Int
    | TogglePageSizeDropdown
    | ClosePageSizeDropdown
    | ClosePageSizeDropdownHack
    | FilterTextChange String
    | FilterTypeChange ColumnFilter
    | ToggleFilterDropdown
    | CloseFilterDropdown
    | ToggleColumn msg Bool
    | ToggleColumnSettings
    | CloseColumnSettings
    | SpitOut msg


update : Model -> Msg a msg -> ( Model, Cmd (Msg a msg), Maybe msg )
update model subMsg =
    case subMsg of
        NoOp ->
            ( model, Cmd.none, Nothing )

        PageSizeChanged val ->
            noOut (handlePageSizeChanged model val)

        TogglePageSizeDropdown ->
            ( { model | itemsPerPageOpenness = UI.flipOpenness model.itemsPerPageOpenness }
            , focusPageSizeOptions model
            , Nothing
            )

        ClosePageSizeDropdown ->
            ( { model | itemsPerPageOpenness = Closed }
            , Cmd.none
            , Nothing
            )

        ClosePageSizeDropdownHack ->
            ( model
            , Process.sleep 100
                |> Task.perform (always ClosePageSizeDropdown)
            , Nothing 
            )

        SortBy sort ->
            noOut (updateSort model sort)

        UpdatePage incr ->
            noOut (incrementPage model incr)

        GotoPage upPage ->
            noOut (gotoPage model upPage)

        FilterTextChange text ->
            noOut (filterTextChange model text)

        FilterTypeChange column ->
            noOut (filterTypeChange model column)

        ToggleFilterDropdown ->
            ( { model | filterDropdownOpenness = UI.flipOpenness model.filterDropdownOpenness }
            , focusFilterOptions model
            , Nothing
            )

        CloseFilterDropdown ->
            ( { model | filterDropdownOpenness = Closed }
            , Cmd.none
            , Nothing
            )

        ToggleColumn msg checked ->
            ( model, Cmd.none, Just msg )

        ToggleColumnSettings ->
            noOut ( { model | columnSettingsOpenness = UI.flipOpenness model.columnSettingsOpenness }, Cmd.none )

        CloseColumnSettings ->
            ( { model | columnSettingsOpenness = Closed }
            , Cmd.none
            , Nothing
            )

        SpitOut msg ->
            ( model, Cmd.none, Just msg )


noOut : ( Model, Cmd (Msg a msg) ) -> ( Model, Cmd (Msg a msg), Maybe msg )
noOut ( model, cmd ) =
    ( model, cmd, Nothing )


updateSort : Model -> String -> ( Model, Cmd (Msg a msg) )
updateSort model sort =
    let
        newSort =
            case model.sort of
                Sort_None ->
                    Sort sort

                Sort sortedBy ->
                    if sort == sortedBy then
                        Sort_Reverse sort

                    else
                        Sort sort

                Sort_Reverse sortedBy ->
                    if sort == sortedBy then
                        Sort_None

                    else
                        Sort sort
    in
    ( { model | sort = newSort }
    , Cmd.none
    )


handlePageSizeChanged : Model -> Int -> ( Model, Cmd (Msg a msg) )
handlePageSizeChanged model newSize =
    let
        newModel =
            { model
                | itemsPerPage = newSize
            }

        newCmds =
            blurPageSizeOptions model
    in
    ( newModel
    , newCmds
    )


incrementPage : Model -> Int -> ( Model, Cmd (Msg a msg) )
incrementPage model incr =
    let
        oldPage =
            model.currentPage

        upPage =
            oldPage + incr

        newPage =
            upPage

        newModel =
            { model
                | currentPage = newPage
            }

        newCmds =
            Cmd.none
    in
    ( newModel
    , newCmds
    )


gotoPage : Model -> Int -> ( Model, Cmd (Msg a msg) )
gotoPage model page =
    let
        newModel =
            { model
                | currentPage = page
            }

        newCmds =
            Cmd.none
    in
    ( newModel
    , newCmds
    )


filterTextChange : Model -> String -> ( Model, Cmd (Msg a msg) )
filterTextChange model text =
    let
        newSearch =
            if String.length text > 0 then
                Just text

            else
                Nothing

        newModel =
            { model | search = newSearch }

        newCmd =
            Cmd.none
    in
    ( newModel
    , newCmd
    )


filterTypeChange : Model -> ColumnFilter -> ( Model, Cmd (Msg a msg) )
filterTypeChange model column =
    let
        newColumn =
            if column.title == "All" then
                Nothing

            else
                Just column

        newModel =
            { model
                | filterColumn = newColumn
            }

        newCmd =
            blurFilterOptions model
    in
    ( newModel
    , newCmd
    )



--
-- FILTER VIEWS
--


filterView :
    { config
        | device : Device
        , windowSize : WindowSize
        , panelDimensions : PanelDimensions
        , columnToggles : ColumnToggles msg
    }
    -> Model
    -> Element (Msg a msg)
filterView config model =
    let
        scaleFont =
            UI.scaleFont config.windowSize
    in
    row
        [ spacing 20 ]
        [ row [ spacing 10 ]
            [ el
                [ Font.color UI.white
                , Font.size (scaleFont 1)
                , Font.semiBold
                , UI.montserrat
                ]
              <|
                text "Filter By"
            , filterDropdown config model
            ]
        , el [ width fill ] <|
            Input.search
                [ width <|
                    if UI.is1080p config.windowSize then
                        fill |> minimum 256

                    else
                        shrink
                , height (UI.inputHeight config)
                , Font.size (scaleFont 1)
                , Font.color UI.white
                , Bg.color UI.lunarGreen
                , Border.color UI.lunarGreen
                , UI.id inputIds.filterSearch
                ]
                { onChange = FilterTextChange
                , text = Maybe.withDefault "" model.search
                , placeholder =
                    Just <|
                        Input.placeholder [ paddingXY 12 0, height shrink, centerY ] <|
                            el [] <|
                                text "Enter a value to filter by"
                , label = Input.labelHidden "Search By"
                }
        ]


filterDropdown :
    { config
        | device : Device
        , windowSize : WindowSize
        , panelDimensions : PanelDimensions
        , columnToggles : ColumnToggles msg
    }
    -> Model
    -> Element (Msg a msg)
filterDropdown config { filterDropdownOpenness, filterColumn } =
    let
        options : List ColumnFilter
        options =
            config.columnToggles
                |> List.map (\t -> ColumnFilter t.title)
                |> List.append [ ColumnFilter "All" ]

        optionConfig : OptionConfig ColumnFilter (Msg a msg)
        optionConfig =
            defaultOptionConfig config.windowSize { titleGetter = .title }

        optionListConfig : OptionListConfig ColumnFilter (Msg a msg)
        optionListConfig =
            defaultOptionListConfig config
                { id = inputIds.filterOptions
                , onLoseFocus = CloseFilterDropdown
                , onChange = FilterTypeChange
                , selected = Just <| Maybe.withDefault (ColumnFilter "All") filterColumn
                , label = "Filter Options"
                }

        labelConfig : LabelConfig (Msg a msg)
        labelConfig =
            { otherAttrs = [ spacingXY 6 0 ]
            , label = filterColumn |> Maybe.map .title |> Maybe.withDefault "All"
            , openIcon = Just "fa fa-times"
            , closedIcon = Just "fa fa-caret-down"
            }

        buttonConfig : ButtonConfig (Msg a msg)
        buttonConfig =
            defaultButtonConfig config.windowSize
                { id = inputIds.filterButton
                , width =
                    if UI.is1080p config.windowSize then
                        fill |> minimum 256

                    else
                        fill |> minimum 220
                , height = UI.inputHeight config
                , onPress = Just ToggleFilterDropdown
                }

        selectConfig : SelectConfig ColumnFilter (Msg a msg)
        selectConfig =
            defaultSelectConfig config.windowSize
                { buttonConfig = buttonConfig
                , labelConfig = labelConfig
                , optionListConfig = optionListConfig
                , optionConfig = optionConfig
                , direction = SelectDown
                }
    in
    select selectConfig filterDropdownOpenness options



--
-- COLUMN TOGGLES VIEWS
--


columnTogglesView :
    { config
        | windowSize : WindowSize
        , panelDimensions : PanelDimensions
        , columnToggles : ColumnToggles msg
    }
    -> Model
    -> Element (Msg a msg)
columnTogglesView config model =
    let
        scaleFont =
            UI.scaleFont config.windowSize

        checkboxRow toggle =
            Input.checkbox
                [ paddingXY 6 8
                , Font.size (scaleFont 1)
                , Font.color UI.white
                , width (fill |> minimum 268)
                ]
                { onChange = ToggleColumn toggle.toggleMessage
                , icon = UI.checkboxIcon
                , checked = toggle.shown
                , label = Input.labelRight [] <| text toggle.title
                }

        checkboxes =
            column
                [ height <| px 200
                , clip
                ]
                [ row
                    [ height (px 32)
                    , width fill
                    , Bg.color UI.pineTree
                    , Border.roundEach { corners | topLeft = 3, topRight = 3 }
                    , Font.color UI.white
                    , Font.size (scaleFont 2)
                    , alignLeft
                    , paddingXY 8 0
                    ]
                    [ text "Show/Hide Columns"
                    ]
                , column [ scrollbarY ] <|
                    List.map checkboxRow config.columnToggles
                ]
    in
    el
        ([ height fill
         , width fill
         ]
            ++ (case model.columnSettingsOpenness of
                    Open ->
                        [ above <|
                            el
                                [ height (fill |> minimum 200)
                                , Bg.color UI.transparent70
                                , moveRight 62
                                , onLeft <|
                                    el
                                        [ width fill
                                        , Border.roundEach { corners | topLeft = 3, topRight = 3 }
                                        , Bg.color UI.transparent70
                                        ]
                                        checkboxes
                                ]
                                none
                        ]

                    Closed ->
                        []
               )
        )
    <|
        Input.button
            [ Font.color UI.white
            , UI.title "Show/Hide Columns"
            , Bg.color UI.lunarGreen
            , height fill
            , width <| px 62
            , UI.id inputIds.showHideButton
            ]
            { onPress = Just ToggleColumnSettings
            , label =
                el
                    [ width fill
                    , paddingXY 22 4
                    , Border.widthEach <| { borders | right = 2 }
                    , Border.color UI.white
                    , Border.solid
                    ]
                <|
                    UI.centeredIcon "fa fa-ellipsis-h"
            }



--
-- GRID VIEWS
--


gridView :
    { config
        | device : Device
        , windowSize : WindowSize
        , panelDimensions : PanelDimensions
        , columnDefs : ColumnDefinitions a msg
        , data : List a
    }
    -> Model
    -> Element (Msg a msg)
gridView config model =
    let
        visibleColumns =
            List.filter .shown config.columnDefs

        filteredData =
            filterColumns model config.columnDefs config.data
    in
    column [ width fill, height fill ]
        [ gridBodyWithData
            { device = config.device
            , windowSize = config.windowSize
            , panelDimensions = config.panelDimensions
            , visibleColumns = visibleColumns
            , filteredData = filteredData
            }
            model
        , gridFooterWithData config model filteredData
        ]


gridBodyWithData :
    { config
        | device : Device
        , windowSize : WindowSize
        , panelDimensions : PanelDimensions
        , visibleColumns : ColumnDefinitions a msg
        , filteredData : List a
    }
    -> Model
    -> Element (Msg a msg)
gridBodyWithData config model =
    let
        renderData =
            getRenderDataFrom model config.visibleColumns config.filteredData

        gridHeight =
            config.panelDimensions.bottomDrawerHeight
                - config.panelDimensions.bottomDrawerFooterHeight
                - config.panelDimensions.bottomDrawerHeaderHeight
    in
    row [ width fill ]
        [ indexedTable [ width fill, height (px gridHeight), clip, scrollbars ]
            { data = renderData
            , columns = List.map (columnDefToColumn config model.sort) config.visibleColumns
            }
        ]


columnDefToColumn :
    { config
        | device : Device
        , windowSize : WindowSize
    }
    -> SortBy
    -> ColumnDefinition a msg
    -> IndexedColumn a (Msg a msg)
columnDefToColumn config sort definition =
    let
        rowHeight =
            UI.adjustOnHeight config.windowSize config.device ( 22, 31 )
    in
    { header = tableHeaderUi config rowHeight sort definition
    , width = fill
    , view = tableCellUi config rowHeight sort definition
    }


tableHeaderUi :
    { config
        | device : Device
        , windowSize : WindowSize
    }
    -> Int
    -> SortBy
    -> ColumnDefinition a msg
    -> Element (Msg a msg)
tableHeaderUi config rowHeight sort definition =
    let
        sortClass =
            sortByIconClass sort definition

        btnTitle =
            case sort of
                Sort_None ->
                    "Sort ascending (" ++ definition.title ++ ")"

                Sort a ->
                    "Sort descending (" ++ definition.title ++ ")"

                Sort_Reverse a ->
                    "Clear sort (" ++ definition.title ++ ")"
    in
    Input.button
        [ Bg.color UI.pineTree
        , height <| px rowHeight
        , width fill
        , paddingXY 5 0
        , UI.title btnTitle
        ]
        { onPress = Just <| SortBy definition.title
        , label =
            row
                [ height fill
                , width fill
                , spacingXY 5 0
                , Font.color UI.white
                , Font.size <| UI.scaleFont config.windowSize -1
                , UI.helvetica
                , Font.bold
                ]
                [ text definition.title
                , el [ alignLeft, centerY ] <| UI.icon sortClass
                ]
        }


tableCellUi :
    { config
        | device : Device
        , windowSize : WindowSize
    }
    -> Int
    -> SortBy
    -> ColumnDefinition a msg
    -> Int
    -> a
    -> Element (Msg a msg)
tableCellUi config rowHeight sort { getter } index item =
    let
        bgColor =
            if modBy 2 index == 0 then
                UI.white

            else
                UI.satinLinen
    in
    el
        [ UI.title <| tableDataTitle getter item
        , height <| px rowHeight
        , paddingXY 5 0
        , width fill
        , Bg.color bgColor
        , Font.color UI.doveGray
        , Font.size <| UI.scaleFont config.windowSize -2
        , UI.helvetica
        ]
    <|
        tableDataUi getter item


tableDataTitle : Getter a msg -> a -> String
tableDataTitle getter item =
    case getter of
        StringGetter get ->
            get item

        IntGetter get ->
            get item |> String.fromInt

        FloatGetter get ->
            get item |> String.fromFloat

        DynamicGetter get ->
            get item |> Tuple.second


tableDataUi : Getter a msg -> a -> Element (Msg a msg)
tableDataUi getter item =
    case getter of
        StringGetter get ->
            el [ centerY ] <| text <| get item

        IntGetter get ->
            el [ centerY ] <| text <| String.fromInt (get item)

        FloatGetter get ->
            el [ centerY ] <| text <| String.fromFloat (get item)

        DynamicGetter get ->
            get item
                |> Tuple.first
                |> map SpitOut



--
-- GRID FOOTER VIEWS
--


gridFooterWithData :
    { config
        | device : Device
        , windowSize : WindowSize
        , panelDimensions : PanelDimensions
        , data : List a
    }
    -> Model
    -> List a
    -> Element (Msg a msg)
gridFooterWithData config model filteredData =
    row
        [ alignBottom
        , height (px config.panelDimensions.bottomDrawerFooterHeight)
        , width fill
        ]
        [ paginationControls config model filteredData
        ]


paginationControls :
    { config
        | device : Device
        , windowSize : WindowSize
        , panelDimensions : PanelDimensions
        , data : List a
    }
    -> Model
    -> List a
    -> Element (Msg a msg)
paginationControls config model filteredData =
    let
        filteredCount =
            List.length filteredData

        totalCount =
            List.length config.data

        pageStart =
            model.currentPage * model.itemsPerPage + 1

        pageMax =
            (model.currentPage + 1) * model.itemsPerPage

        pageEnd =
            if pageMax <= filteredCount then
                pageMax

            else
                filteredCount

        filteredMax =
            maxPageOf filteredData model.itemsPerPage
    in
    row
        [ padding 10
        , spacing 20
        , width fill
        ]
        [ pageSizeSelector config model
        , currentPageDisplay pageStart pageEnd filteredCount
        , pageNavigation pageMax filteredCount filteredMax model
        ]


pageSizeSelector :
    { config
        | device : Device
        , windowSize : WindowSize
        , panelDimensions : PanelDimensions
    }
    -> Model
    -> Element (Msg a msg)
pageSizeSelector config { itemsPerPageOpenness, itemsPerPage } =
    let
        options : List Int
        options =
            [ 10, 25, 50, 100 ]

        optionConfig : OptionConfig Int (Msg a msg)
        optionConfig =
            defaultOptionConfig config.windowSize { titleGetter = String.fromInt }

        optionListConfig : OptionListConfig Int (Msg a msg)
        optionListConfig =
            defaultOptionListConfig config
                { id = inputIds.pageSizeOptions
                , onLoseFocus = ClosePageSizeDropdownHack
                , onChange = PageSizeChanged
                , selected = Just itemsPerPage
                , label = "Page Size Options"
                }

        labelConfig : LabelConfig (Msg a msg)
        labelConfig =
            { otherAttrs = []
            , label = String.fromInt itemsPerPage
            , openIcon = Just "fa fa-times"
            , closedIcon = Just "fa fa-caret-up"
            }

        buttonConfig : ButtonConfig (Msg a msg)
        buttonConfig =
            defaultButtonConfig config.windowSize
                { id = inputIds.pageSizeButton
                , width = shrink |> minimum 55
                , height = UI.inputHeight config
                , onPress = Just TogglePageSizeDropdown
                }

        selectConfig : SelectConfig Int (Msg a msg)
        selectConfig =
            defaultSelectConfig config.windowSize
                { buttonConfig = buttonConfig
                , labelConfig = labelConfig
                , optionListConfig = optionListConfig
                , optionConfig = optionConfig
                , direction = SelectUp
                }
    in
    row [ height fill, spacing 10 ]
        [ el
            [ Font.color UI.white
            , Font.size (UI.scaleFont config.windowSize 1)
            , Font.semiBold
            , UI.montserrat
            ]
          <|
            text "Items Per Page"
        , select selectConfig itemsPerPageOpenness options
        ]


currentPageDisplay : Int -> Int -> Int -> Element (Msg a msg)
currentPageDisplay pageStart pageEnd filteredCount =
    el
        [ Font.color UI.white
        , Font.size 14
        , Font.semiBold
        , UI.montserrat
        ]
    <|
        text
            (String.fromInt
                pageStart
                ++ " - "
                ++ String.fromInt
                    pageEnd
                ++ " of "
                ++ String.fromInt
                    filteredCount
            )


pageNavigation : Int -> Int -> Int -> Model -> Element (Msg a msg)
pageNavigation pageMax filteredCount filteredMax model =
    row
        [ Font.color UI.white
        , Font.size 14
        , spacing 12
        ]
        [ Input.button
            [ UI.title "First page" ]
            { onPress =
                if model.currentPage > 0 then
                    Just (GotoPage 0)

                else
                    Nothing
            , label = UI.centeredIcon "fa fa-step-backward"
            }
        , Input.button
            [ UI.title "Previous page" ]
            { onPress =
                if model.currentPage > 0 then
                    Just (UpdatePage -1)

                else
                    Nothing
            , label = UI.centeredIcon "fa fa-caret-left"
            }
        , Input.button
            [ UI.title "Next page" ]
            { onPress =
                if pageMax < filteredCount then
                    Just (UpdatePage 1)

                else
                    Nothing
            , label = UI.centeredIcon "fa fa-caret-right"
            }
        , Input.button
            [ UI.title "Last page" ]
            { onPress =
                if pageMax < filteredCount then
                    Just (GotoPage filteredMax)

                else
                    Nothing
            , label = UI.centeredIcon "fa fa-step-forward"
            }
        ]



--
-- HELPER FUNCTIONS
--


inputIds =
    { filterButton = "filter_button"
    , filterOptions = "filter_options"
    , filterSearch = "filter_search"
    , showHideButton = "show_hide_button"
    , showHideOptions = "show_hide_options"
    , pageSizeButton = "page_size_button"
    , pageSizeOptions = "page_size_options"
    }


focusFilterOptions : Model -> Cmd (Msg a msg)
focusFilterOptions model =
    case model.filterDropdownOpenness of
        Open ->
            Cmd.none

        Closed ->
            Task.attempt (\_ -> NoOp) (Dom.focus inputIds.filterOptions)


blurFilterOptions : Model -> Cmd (Msg a msg)
blurFilterOptions model =
    case model.filterDropdownOpenness of
        Open ->
            Task.attempt (\_ -> NoOp) (Dom.blur inputIds.filterOptions)

        Closed ->
            Cmd.none


focusPageSizeOptions : Model -> Cmd (Msg a msg)
focusPageSizeOptions model =
    case model.itemsPerPageOpenness of
        Open ->
            Cmd.none

        Closed ->
            Task.attempt (\_ -> NoOp) (Dom.focus inputIds.pageSizeOptions)


blurPageSizeOptions : Model -> Cmd (Msg a msg)
blurPageSizeOptions model =
    case model.itemsPerPageOpenness of
        Open ->
            Task.attempt (\_ -> NoOp) (Dom.blur inputIds.pageSizeOptions)

        Closed ->
            Cmd.none


sortByIconClass : SortBy -> ColumnDefinition a msg -> String
sortByIconClass sort definition =
    case sort of
        Sort_None ->
            ""

        Sort col ->
            if definition.title == col then
                "fa fa-sort-up"

            else
                ""

        Sort_Reverse col ->
            if definition.title == col then
                "fa fa-sort-down"

            else
                ""


maxPageOf : List a -> Int -> Int
maxPageOf data itemsPerPage =
    let
        length =
            List.length data - 1

        max =
            floor (toFloat length / toFloat itemsPerPage)
    in
    max


filterColumns : Model -> ColumnDefinitions a msg -> List a -> List a
filterColumns model columnDefs data =
    case model.search of
        Just search ->
            let
                lowerSearch =
                    String.toLower search

                columns =
                    case model.filterColumn of
                        Just column ->
                            List.filter (\a -> a.title == column.title) columnDefs

                        Nothing ->
                            columnDefs
            in
            data
                |> List.filter
                    (\item ->
                        columns
                            |> List.map
                                (\column ->
                                    (case column.getter of
                                        StringGetter getter ->
                                            String.toLower (getter item)

                                        IntGetter getter ->
                                            String.fromInt (getter item)

                                        FloatGetter getter ->
                                            String.fromFloat (getter item)

                                        DynamicGetter getter ->
                                            let
                                                ( _, searchable ) =
                                                    getter item
                                            in
                                            String.toLower searchable
                                    )
                                        |> String.contains lowerSearch
                                )
                            |> List.any identity
                    )

        Nothing ->
            data


getSorter : Model -> ColumnDefinitions a msg -> String -> Maybe (Sorter a)
getSorter model columnDefs by =
    case
        List.head (List.filter (\a -> a.title == by) columnDefs)
    of
        Just column ->
            Just column.sort

        Nothing ->
            Nothing


getSortedDataFrom : Model -> ColumnDefinitions a msg -> List a -> List a
getSortedDataFrom model columnDefs data =
    case model.sort of
        Sort_None ->
            data

        Sort by ->
            let
                gotted_sorter =
                    getSorter model columnDefs by
            in
            case gotted_sorter of
                Just sorter ->
                    sorter.order data

                Nothing ->
                    data

        Sort_Reverse by ->
            let
                gotted_sorter =
                    getSorter model columnDefs by
            in
            List.reverse
                (case gotted_sorter of
                    Just sorter ->
                        sorter.order data

                    Nothing ->
                        data
                )


getRenderDataFrom : Model -> ColumnDefinitions a msg -> List a -> List a
getRenderDataFrom model columnDefs data =
    List.take model.itemsPerPage
        (List.drop
            (model.currentPage * model.itemsPerPage)
            (getSortedDataFrom model
                columnDefs
                data
            )
        )
