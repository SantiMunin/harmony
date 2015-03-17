import requests, json

{{#schema}}
class {{schemaName}}:

    def __init__(self, info):
        self._info = info

    @classmethod
    def fromJSON(self, text):
        return {{schemaName}}(json.loads(text))

    def toJSON(self):
        return json.dumps(self._info)

{{#schemaVars}}
    def get_{{varName}}(self):
        return self._info["{{varName}}"]

    def set_{{varName}}(self, {{varName}}):
        self._info["{{varName}}"] = {{varName}}

{{/schemaVars}}
{{/schema}}

{{#schema}}
def get{{schemaName}}_list(url):
    return requests.get(url + "{{&schemaRoute}}")

def get{{schemaName}}(url, item_id):
    return requests.get(url + "{{&schemaRoute}}" + "/" + item_id)

def put{{schemaName}}(url, item):
    return requests.put(url + "{{&schemaRoute}}", data=item.toJSON(), headers = {'content-type': 'application/json'})

def post{{schemaName}}(url, item):
    return requests.post(url + "{{&schemaRoute}}", data=item.toJSON(), headers = {'content-type': 'application/json'})

def delete{{schemaName}}(url, item_id):
    return requests.delete(url + "{{&schemaRoute}}" + "/" + item_id)
{{/schema}}
