from client import *
from hypothesis import *
from hypothesis.specifiers import * 
import unittest

Settings.default.max_examples = 100
Settings.default.timeout = 1

{{#schema}}
{{schemaName}}Data = {
{{#schemaVars}}
  '{{varName}}': {{#isList}}[{{/isList}}{{&varType}}{{#isList}}]{{/isList}}, 
{{/schemaVars}}
}
{{/schema}}

url = "http://localhost:3000"

{{#schema}}
{{#schemaRoute}}
class Test{{schemaName}}(unittest.TestCase):
  @given({{schemaName}}Data, {{schemaName}}Data)
  def test_insert_edit_delete(self, data, data2):
{{#schemaVars}}
    {{varName}} = data["{{varName}}"]
{{/schemaVars}}
    postResponse = post{{schemaName}}(url, {{schemaName}}({{#schemaVars}}{{varName}}, {{/schemaVars}}))
    self.assertEqual(201, postResponse.status_code)
    id = json.loads(postResponse.text)["id"]
{{#schemaVars}}
    self.assertEqual({{varName}}, {{schemaName}}.fromJSON(get{{schemaName}}(url, id).text).get_{{varName}}())
{{/schemaVars}}

{{#schemaVars}}
    {{varName}} = data2["{{varName}}"]
{{/schemaVars}}

    putResponse = put{{schemaName}}(url, id, {{schemaName}}({{#schemaVars}}{{varName}}, {{/schemaVars}}))
    self.assertEqual(200, putResponse.status_code)
{{#schemaVars}}
    self.assertEqual({{varName}}, {{schemaName}}.fromJSON(get{{schemaName}}(url, id).text).get_{{varName}}())
{{/schemaVars}}

    deleteResponse = delete{{schemaName}}(url, id)
    self.assertEqual(200, deleteResponse.status_code)

    getAfterDeleteResponse = get{{schemaName}}(url, id)
    self.assertEqual(404, getAfterDeleteResponse.status_code)

{{/schemaRoute}}
{{/schema}}

if __name__ == '__main__':
    unittest.main()
