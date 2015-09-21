#!/bin/python
# -*- coding: utf-8 -*-

# Auto generado por Harmony
from client import *

def print_op(text, op):
    print(text + " ---> [" + str(op.status_code) + "] " + op.text)
    print("")

url = "http://localhost:8250"

# Itento de operacion sin estar autenticado
print_op("Obteniendo lista de tweets sin estar autenticado", getTweet_list(url, "no_session"))

# Registro
print_op("Registrando usuario", register(url, "user", "pw"))

# Logeo fallido
print_op("Intento de login con credenciales incorrectas", login(url, "user", "wrong_pw"))

# Logeo exitoso
loginResponse = login(url, "user", "pw")
print_op("Intento de login con credenciales correctas", loginResponse)
session_token = json.loads(loginResponse.text)["token"]

# Obtener lista de tweets
print_op("Obteniendo lista de tweets", getTweet_list(url, session_token))

# Crear tweet
create_tweet = postTweet(url, Tweet(content="Hello Simple Twitter!", kind="NORMAL", timestamp=123321313), session_token)
tweet_id = json.loads(create_tweet.text)["id"]
print_op("Creando tweet", create_tweet)

# Obtener lista de tweets (esta vez con contenido)
print_op("Obteniendo list de tweets", getTweet_list(url, session_token))

# Modificar tweet
print_op("Modificando tweet", putTweet(url, tweet_id, Tweet(author="NewAuthor", kind="PROMOTED", content="Hello again!", timestamp=1337), session_token))

# Obtener tweet concreto
print_op("Obteniendo tweet con id " + tweet_id, getTweet(url, tweet_id, session_token))

# Borrar tweet
print_op("Borrando tweet con id " + tweet_id, deleteTweet(url, tweet_id, session_token))

# Obtener lista de tweets (esta vez con contenido)
print_op("Obteniendo tweet con id " + tweet_id, getTweet(url, tweet_id, session_token))

# Crear topic list
print_op("Creando lista", putTopicList(url, "Software Engineering", TopicList(accounts = ["JeffDean", "RobPike", "SimonMarlow"]), session_token))

# Obtener topic list
print_op("Obteniendo lista", getTopicList_list(url, session_token))

