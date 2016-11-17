module Plugins.PluginDispatcher exposing (..)

import Html exposing (..)
import Plugins.SimplePlugin as SimplePlugin
import Plugins.AdvancedPlugin as AdvancedPlugin


type Plugin
    = SimplePlugin SimplePlugin.Model
    | AdvancedPlugin AdvancedPlugin.Model


type PluginMessage
    = SimplePluginMessage SimplePlugin.Msg
    | AdvancedPluginMessage AdvancedPlugin.Msg


getPlugin : String -> Maybe Plugin
getPlugin config =
    case config of
        "12" ->
            Just (SimplePlugin SimplePlugin.init)

        "21" ->
            Just (AdvancedPlugin AdvancedPlugin.init)

        _ ->
            Nothing


update : PluginMessage -> Plugin -> ( Plugin, Cmd PluginMessage )
update msg plugin =
    case ( msg, plugin ) of
        ( AdvancedPluginMessage msg, AdvancedPlugin model ) ->
            let
                ( advancedPluginModel, advancedPluginMessage ) =
                    AdvancedPlugin.update msg model
            in
                AdvancedPlugin advancedPluginModel ! [ Cmd.map AdvancedPluginMessage advancedPluginMessage ]

        ( SimplePluginMessage msg, SimplePlugin model ) ->
            SimplePlugin (SimplePlugin.update msg model) ! []

        _ ->
            plugin ! []


tagMessage : (PluginMessage -> a) -> (b -> PluginMessage) -> b -> a
tagMessage parentMsg pluginMsg actualPluginMsg =
    parentMsg (pluginMsg actualPluginMsg)


view : (PluginMessage -> a) -> Plugin -> Html a
view parentMsg plugin =
    case plugin of
        SimplePlugin data ->
            Html.map (tagMessage parentMsg SimplePluginMessage) (SimplePlugin.view data)

        AdvancedPlugin data ->
            Html.map (tagMessage parentMsg AdvancedPluginMessage) (AdvancedPlugin.view data)
