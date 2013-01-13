GameCloud
=========
***

##RESTful API

###Resources

####Developer collection - **http://54.245.119.57:8080/developer**

CREATE:

~~~
$ curl -H "Content-Type: application/json" -X POST -d '{"id":"dev", "email":"dev@com","password":"pass"}' 54.245.119.57:8080/developer/
~~~

####Developer - **http://54.245.119.57:8080/developer/DEV_ID**

READ:

~~~
$ curl -u dev:pass -H "Content-Type: application/json" -X GET 54.245.119.57:8080/developer/dev

{"id":"dev","email":"dev@com"}
~~~

DELETE:

~~~
$ curl -u dev:pass -H "Content-Type: application/json" -X DELETE 54.245.119.57:8080/developer/dev
~~~

####Game collection - **http://54.245.119.57:8080/developer/DEV_ID/game**

CREATE:

~~~
$ curl -u dev:pass -H "Content-Type: application/json" -X POST -d '{"name":"sc2", "description":"sadasd","platform":"ios","game_link":"game_link","market_link":"market_link"}' 54.245.119.57:8080/developer/dev/game

{"game_key":"e2277af1cce79ef3c334cbacb400027b"}
~~~

READ:

~~~
$ curl -u dev:pass -H "Content-Type: application/json" -X GET 54.245.119.57:8080/developer/dev/game

[{"key":"e2277af1cce79ef3c334cbacb4000c6f","name":"sc3","description":"sadasdsss","game_link":"game_link","platform":"android","market_link":"market_link"},{"key":"e2277af1cce79ef3c334cbacb400027b","name":"sc2","description":"sadasd","game_link":"game_link","platform":"ios","market_link":"market_link"}]
~~~

####Game -  **http://54.245.119.57:8080/developer/DEV_ID/game/GAME_KEY**

READ:

~~~
$ curl -u dev:pass -H "Content-Type: application/json" -X GET 54.245.119.57:8080/developer/dev/game/e2277af1cce79ef3c334cbacb4000c6f

{"key":"e2277af1cce79ef3c334cbacb4000c6f","name":"sc3","description":"sadasdsss","game_link":"game_link","platform":"android","market_link":"market_link"}
~~~

DELETE:

~~~
$ curl -u dev:pass -H "Content-Type: application/json" -X DELETE 54.245.119.57:8080/developer/dev/game/e2277af1cce79ef3c334cbacb4000c6f
~~~