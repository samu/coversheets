module Plugins.PluginDispatcher exposing (..)

import Html exposing (..)
import Html.App as App
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


update : PluginMessage -> Plugin -> Plugin
update msg plugin =
    case ( msg, plugin ) of
        ( AdvancedPluginMessage msg, AdvancedPlugin model ) ->
            AdvancedPlugin (AdvancedPlugin.update msg model)

        ( SimplePluginMessage msg, SimplePlugin model ) ->
            SimplePlugin (SimplePlugin.update msg model)

        _ ->
            plugin


tagMessage : (PluginMessage -> a) -> (b -> PluginMessage) -> b -> a
tagMessage parentMsg pluginMsg actualPluginMsg =
    parentMsg (pluginMsg actualPluginMsg)


view : (PluginMessage -> a) -> Plugin -> Html a
view parentMsg plugin =
    case plugin of
        SimplePlugin data ->
            App.map (tagMessage parentMsg SimplePluginMessage) (SimplePlugin.view data)

        AdvancedPlugin data ->
            App.map (tagMessage parentMsg AdvancedPluginMessage) (AdvancedPlugin.view data)
