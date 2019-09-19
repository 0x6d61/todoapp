import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox {init = init,update = update,view = view}


type alias Todo = 
    {
        taskName:String
    }

type alias Model = 
    {
        newTask: Todo,
        todoList:List Todo
    }
    

init : Model
init = 
    {   newTask = {taskName = ""},
        todoList =  [
        ]
    }


type Msg
    = Add String
    | Delete String
    | Submit
                


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add task ->
            {model |  newTask =  {taskName = task} }
        Submit ->
            {model | todoList = model.todoList ++ [model.newTask]}
        Delete task ->
            let
                n = taskSearch task model.todoList 0
            in 
                {model | todoList = List.take n model.todoList ++ List.drop  (n+1) model.todoList}
    
view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ div [ class "addTask"]
                [
                    input [class "addTaskInput" ,type_ "text" ,placeholder "タスク" , onInput Add , value model.newTask.taskName] [],
                    button [ class "addTaskButton",onClick Submit ] [text "追加"]
        ,div [ class "todoList" ]
            [
                viewTaskList model.todoList
            ]
        ]
    ]

viewTaskList : List Todo -> Html Msg
viewTaskList taskList = 
    ul [class "task"]
        (List.map viewItem taskList)
taskSearch : String -> List Todo -> Int -> Int
taskSearch task list n =
    case list of
        [] -> n
        s :: lst -> 
            if task == s.taskName then
                n
            else
                taskSearch task lst n+1

viewItem : Todo -> Html Msg
viewItem task = 
    li [] [
        text task.taskName,
        button [class "addTaskButton",onClick (Delete task.taskName)] [text "削除"],
        hr [] []
    ]