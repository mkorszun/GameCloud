GameCloud
=========
***

###Resources

* Auth: Basic | Token based
* URL: gamecloudio.com

####Token

#####Create

~~~bash
$ curl -u [DEVELOPER_ID]:[PASSWORD]  -H "Accept: application/json" -X GET http://gamecloudio.com/token
{"token":"B33BFE9367175983107B7B34A4A86977C7C45FB4"}
~~~

Use token in Authorization header instead:

~~~bash
$ curl -H "Accept: application/json" -H "Authorization: gc_auth_token B33BFE9367175983107B7B34A4A86977C7C45FB4" -X GET localhost:8080/developer/mateusz
~~~

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
    "platform" : "android" | "ios",
    "game_link" : "STRING",
    "market_link" : "STRING",
    "tags" : ["STRING", "STRING"],
    "status" : "new" | "beta" | "published",
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

#####READ_ALL:

~~~bash
$ curl -u [DEVELOPER_ID]:[PASSWORD] -H "Accept: application/json" -X GET http://gamecloudio.com/developer/[DEVELOPER_ID]/game
~~~

#####READ:

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