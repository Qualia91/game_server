<h1 align='center'>
  GAME SERVER
</h1>

<p align='center'>
  <img src="https://img.shields.io/badge/License-MIT-blue.svg"/>
  <img src="https://badgen.net/badge/Open%20Source%20%3F/Yes%21/blue?icon=github)](https://github.com/Naereen/badges/"/>
</p>

<p align='center'>
  <img src="https://img.shields.io/badge/Erlang-00ADD8?logo=erlang&logoColor=white" />
</p>

<details>
  <summary><b>Introduction</b></summary>
Welcome to the erlang turned based game server. We use cowboy websocket connections to provide a realtime game server for turn based games. As Godot Game Engine is the best game engine out there (completely unbiased and correct opinion), we have also provided a nice gd script you can place into your game to use the server to hide some of the innerds (WSGameClient.gd). Be sure to let me know if you end up using it! 
</details>

<details>
  <summary><b>Design</b></summary>
  <img alt="Design Diagram" src="images/design_diagram.svg">
</details>

<details>
  <summary><b>Usage</b></summary>
  
This application is fully dockerised, and can be run using the following commands locally.

## Docker build container
docker build -t game_server .

## Docker start container
docker run -d -p 8080:8080 --init game_server

## Docker open shell
docker run -itp 8080:8080 --init game_server sh

</details>
