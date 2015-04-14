import requests, json
{{#schema}}

class {{schemaName}}:

    def __init__(self, {{#schemaVars}}{{varName}}, {{/schemaVars}}):
        self._info = {}
{{#schemaVars}}
{{#isStruct}}
{{#isList}}
        self._info["{{varName}}"] = map((lambda x: x if isinstance(x, dict) else x.toDict()), {{varName}})
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

{{#schema}}{{#schemaRoute}}

def get{{schemaName}}_list(url):
    return requests.get(url + "{{&value}}")

def get{{schemaName}}(url, item_id):
    return requests.get(url + "{{&value}}" + "/" + item_id)
{{#writable}}

{{#hasKeyField}}

def put{{schemaName}}(url, item):
    return requests.put(url + "{{&value}}" + "/" + item.get_{{keyField}}(), data=item.toJSON(), headers = {'content-type': 'application/json'})
{{/hasKeyField}}

def put{{schemaName}}(url, item_id, item):
    return requests.put(url + "{{&value}}" + "/" + item_id, data=item.toJSON(), headers = {'content-type': 'application/json'})
{{^hasKeyField}}

def post{{schemaName}}(url, item):
    return requests.post(url + "{{&value}}", data=item.toJSON(), headers = {'content-type': 'application/json'})
{{/hasKeyField}}

def delete{{schemaName}}(url, item_id):
    return requests.delete(url + "{{&value}}" + "/" + item_id)
{{/writable}}
{{/schemaRoute}}{{/schema}}
