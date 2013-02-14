GameCloud
=========
***

###Resources

Auth: Basic
URL: gamecloudio.com

####Developer

DEVELOPER_OBJECT:

~~~json
{
    "id" : "STRING",
    "email" : "STRING",
    "password" : "STRING"
}
~~~

#####CREATE:

~~~bash
$ curl -H "Content-Type: application/json" -X POST -d [DEVELOPER_OBJECT] http://gamecloudio.com/developer
~~~

#####READ:

~~~bash
$ curl -u [DEVELOPER_ID]:[PASSWORD]  -H "Accept: application/json" -X GET http://gamecloudio.com/developer/[DEVELOPER_ID]
~~~

#####Update:

~~~bash
$ curl -u [DEVELOPER_ID]:[PASSWORD] -H "Content-Type: application/json" -X PUT -d [VALUES_TO_UPDATE] http://gamecloudio.com/developer/[DEVELOPER_ID]
~~~

VALUES_TU_UPDATE - JSON object containing subset of Developer attributes excluding id (even if specified will be ignored).

#####DELETE:

~~~bash
$ curl -u [DEVELOPER_ID]:[PASSWORD] -X DELETE http://gamecloudio.com/developer/[DEVELOPER_ID]
~~~

####Game collection

GAME_OBJECT:

~~~json
{
    "name" : "STRING",
    "description" : "STRING",
    "platform" : "STRING",
    "game_link" : "STRING",
    "market_link" : "STRING",
    "screen" : {
        "name" : "STRING",
        "content_type" : "STRING",
        "content" : "BASE64_STRING"
    }
}
~~~

#####CREATE:

~~~bash
$ curl -u [DEVELOPER_ID]:[PASSWORD] -H "Content-Type: application/json" -X POST -d [GAME_OBJECT] http://gamecloudio.com/developer/[DEVELOPER_ID]/game
{"game_key": GAME_KEY}
~~~

#####READ:

~~~bash
$ curl -u [DEVELOPER_ID]:[PASSWORD] -H "Accept: application/json" -X GET http://gamecloudio.com/developer/[DEVELOPER_ID]/game
~~~

#####READ ALL:

~~~bash
$ curl -u [DEVELOPER_ID]:[PASSWORD] -H "Accept: application/json" -X GET http://gamecloudio.com/developer/[DEVELOPER_ID]/game/[GAME_KEY]
~~~

#####UPDATE:

~~~bash
$ curl -u [DEVELOPER_ID]:[PASSWORD] -H "Content-Type: application/json" -X PUT -d [VALUES_TO_UPDATE] http://gamecloudio.com/developer/[DEVELOPER_ID]/game/[GAME_KEY]
~~~

VALUES_TU_UPDATE - JSON object containing subset of Game attributes.

#####DELETE:

~~~bash
$ curl -u [DEVELOPER_ID]:[PASSWORD] -X DELETE http://gamecloudio.com/developer/[DEVELOPER_ID]/game/[GAME_KEY]
~~~