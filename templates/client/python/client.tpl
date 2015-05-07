import requests, json
{{#schema}}

class {{schemaName}}:

    def __init__(self, {{#schemaVars}}{{varName}}, {{/schemaVars}}):
        self._info = {}
{{#schemaVars}}
{{#isStruct}}
{{#isList}}
        self._info["{{varName}}"] = map((lambda x: x if (isinstance(x, dict) or isinstance(x, list)) else x.toDict()), {{varName}})
{{/isList}}
{{^isList}}
        self._info["{{varName}}"] = {{varName}}.toDict()
{{/isList}}
{{/isStruct}}
{{^isStruct}}
        self._info["{{varName}}"] = {{varName}}
{{/isStruct}}
{{/schemaVars}}

    @staticmethod
    def fromDict(dict):
      return {{schemaName}}({{#schemaVars}}dict["{{varName}}"],{{/schemaVars}})
    
    def toDict(self):
      return self._info

    @staticmethod
    def fromJSON(text):
        return {{schemaName}}.fromDict(json.loads(text))

    def toJSON(self):
        return json.dumps(self._info)
{{#schemaVars}}

    def get_{{varName}}(self):
        return self._info["{{varName}}"]

    def set_{{varName}}(self, {{varName}}):
        self._info["{{varName}}"] = {{varName}}
{{/schemaVars}}
{{/schema}}

{{#requiresAuth}}
def login(url, login, password):
  return requests.post(url + "/u_auth" + "/" + login + "/" + password)

def register(url , login, password):
  return requests.post(url + "/u_auth/register", data=json.dumps({'login' : login, 'password': password}), headers = {'content-type': 'application/json'})
{{/requiresAuth}}

{{#schema}}{{#schemaRoute}}

def get{{schemaName}}_list(url{{#requiresAuth}}, token{{/requiresAuth}}):
    return requests.get(url + "{{&value}}"{{#requiresAuth}}+ "/" + token{{/requiresAuth}})

def get{{schemaName}}(url, item_id{{#requiresAuth}}, token{{/requiresAuth}}):
    return requests.get(url + "{{&value}}" + "/" + item_id{{#requiresAuth}}+ "/" + token{{/requiresAuth}})
{{#writable}}

{{#hasKeyField}}

def put{{schemaName}}(url, item{{#requiresAuth}}, token{{/requiresAuth}}):
    return requests.put(url + "{{&value}}" + "/" + item.get_{{keyField}}(){{#requiresAuth}}+ "/" + token{{/requiresAuth}}, data=item.toJSON(), headers = {'content-type': 'application/json'})
{{/hasKeyField}}

def put{{schemaName}}(url, item_id, item{{#requiresAuth}}, token{{/requiresAuth}}):
    return requests.put(url + "{{&value}}" + "/" + item_id{{#requiresAuth}}+ "/" + token{{/requiresAuth}}, data=item.toJSON(), headers = {'content-type': 'application/json'})
{{^hasKeyField}}

def post{{schemaName}}(url, item{{#requiresAuth}}, token{{/requiresAuth}}):
    return requests.post(url + "{{&value}}"{{#requiresAuth}}+ "/" + token{{/requiresAuth}}, data=item.toJSON(), headers = {'content-type': 'application/json'})
{{/hasKeyField}}

def delete{{schemaName}}(url, item_id{{#requiresAuth}}, token{{/requiresAuth}}):
    return requests.delete(url + "{{&value}}" + "/" + item_id{{#requiresAuth}}+ "/" + token{{/requiresAuth}})
{{/writable}}
{{/schemaRoute}}{{/schema}}
