module Plugins.PluginDispatcher exposing (..)

import Html exposing (..)
import Html.App as App
import Plugins.SimplePlugin as SimplePlugin
import Plugins.MoreAdvancedPlugin as MoreAdvancedPlugin


type Plugin
    = SimplePlugin SimplePlugin.Model
    | MoreAdvancedPlugin MoreAdvancedPlugin.Model


type PluginMessage
    = SimplePluginMessage SimplePlugin.Msg
    | MoreAdvancedPluginMessage MoreAdvancedPlugin.Msg


getPlugin : String -> Maybe Plugin
getPlugin config =
    case config of
        "12" ->
            Just (SimplePlugin SimplePlugin.init)

        "21" ->
            Just (MoreAdvancedPlugin MoreAdvancedPlugin.init)

        _ ->
            Nothing


update : PluginMessage -> Plugin -> Plugin
update msg plugin =
    case ( msg, plugin ) of
        ( MoreAdvancedPluginMessage msg, MoreAdvancedPlugin model ) ->
            MoreAdvancedPlugin (MoreAdvancedPlugin.update msg model)

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

        MoreAdvancedPlugin data ->
            App.map (tagMessage parentMsg MoreAdvancedPluginMessage) (MoreAdvancedPlugin.view data)
