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
$ curl -H "Accept: application/json" -H "Authorization: gc_auth_token B33BFE9367175983107B7B34A4A86977C7C45FB4" -X GET gamecloudio.com/developer/mateusz
~~~

####Developer

Schema:

~~~json
{
    "description":"A developer",
    "type":"object",
    "properties":{
        "id":{
            "type":"string",
            "required":true
        },
        "email":{
            "type":"string",
            "required":true
        },
        "password":{
            "type":"string",
            "required":true}
    },
    "additionalProperties":false
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
$ curl -u [DEVELOPER_ID]:[PASSWORD] -H "Content-Type: application/json" -X PUT -d [DEVELOPER_OBJECT] http://gamecloudio.com/developer/[DEVELOPER_ID]
~~~

#####DELETE:

~~~bash
$ curl -u [DEVELOPER_ID]:[PASSWORD] -X DELETE http://gamecloudio.com/developer/[DEVELOPER_ID]
~~~

####Game

Schema:

~~~json
{
    "description":"A game",
    "type":"object",
    "properties":{
        "developer_id":{
            "type":"string",
            "required":true
        },
        "name":{
            "type":"string",
            "required":true
        },
        "description":{
            "type":"string",
            "required":true
        },
        "platform":{
            "type":"string",
            "required":true,
            "enum":["ios","android"]
        },
        "game_link":{
            "type":"string",
            "required":true
        },
        "market_link":{
            "type":"string",
            "required":true
        },
        "tags":{
            "type":"array",
            "required":true,
            "items":{"type":"string"}
        },
        "status":{
            "type":"string",
            "required":true,
            "enum":["new","beta","published"]
        },
        "screen":{
            "description":"A screen",
            "type":"object",
            "properties":{
                "name":{
                    "type":"string",
                    "required":true
                },
                "content_type":{
                    "type":"string",
                    "required":true
                },
                "content":{
                    "type":"string",
                    "required":true
                }
            },
            "additionalProperties":false,
            "required":true
        }
    },
    "additionalProperties":false
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
$ curl -u [DEVELOPER_ID]:[PASSWORD] -H "Content-Type: application/json" -X PUT -d [GAME_OBJECT] http://gamecloudio.com/developer/[DEVELOPER_ID]/game/[GAME_KEY]
~~~

#####DELETE:

~~~bash
$ curl -u [DEVELOPER_ID]:[PASSWORD] -X DELETE http://gamecloudio.com/developer/[DEVELOPER_ID]/game/[GAME_KEY]
~~~