from client import *
from hypothesis import *
from hypothesis.specifiers import * 
import unittest
import copy
import sys

class ServiceTest(unittest.TestCase):

    def assertEqual(self, item1, item2):
        i1 = copy.deepcopy(item1)
        i2 = copy.deepcopy(item2)
        if type(i1) != type(i2):
            t1 = type(i1).__name__
            t2 = type(i2).__name__
            # str and unicode can be treated as the same type
            if not ((t1 == "unicode" and t2 == "str") or (t1 == "str" and t2 == "unicode")):
                self.fail("Different types ("+ type(i1).__name__ + "," + type(i2).__name__ + ")")
        if isinstance(i1, dict):
            self.assertEquals_dict(i1, i2)
        elif isinstance(i1, list):
            self.assertEquals_list(i1, i2)
        else:  
            super(ServiceTest, self).assertEqual(i1, i2)
 
    def assertEquals_dict(self, d1, d2):
        self.rm_id_key(d1)
        self.rm_id_key(d2)
        super(ServiceTest, self).assertEqual(d1, d2)

    def assertEquals_list(self, l1, l2):
        i1 = map(self.rm_id_key, l1)
        i2 = map(self.rm_id_key, l2)
        i1.sort()
        i2.sort()
        for i in range(len(i1)):
            self.assertEqual(i1[i], i2[i])

    def rm_id_key(self, item):
        if not isinstance(item, dict):
            return item
        if "id" in item:
            item.pop("id", None)
        return item

Settings.default.max_examples = 100
Settings.default.timeout = 1

{{#schema}}
{{schemaName}}Data = {
{{#schemaVars}}
{{^isKey}}
  '{{varName}}': {{#isList}}[{{/isList}}{{&varType}}{{#isList}}]{{/isList}}, 
{{/isKey}}
{{/schemaVars}}
}
{{/schema}}

url = "http://localhost:3000"

{{#schema}}
{{#schemaRoute}}
class Test{{schemaName}}(ServiceTest):
  @given({{#schemaVars}}{{#isKey}}{{&varType}}, {{/isKey}}{{/schemaVars}}{{schemaName}}Data, {{schemaName}}Data)
  def test_insert_edit_delete(self, {{#hasKeyField}}id, {{/hasKeyField}}data, data2):
{{#schemaVars}}
{{#isKey}}
    {{varName}} = id
{{/isKey}}
{{^isKey}}
    {{varName}} = data["{{varName}}"]
{{/isKey}}
{{/schemaVars}}
{{#hasKeyField}}
    putResponse = put{{schemaName}}(url, id, {{schemaName}}({{#schemaVars}}{{varName}}, {{/schemaVars}}))
    self.assertEqual(200, putResponse.status_code)
{{/hasKeyField}}
{{^hasKeyField}}
    postResponse = post{{schemaName}}(url, {{schemaName}}({{#schemaVars}}{{varName}}, {{/schemaVars}}))
    self.assertEqual(201, postResponse.status_code)
    id = json.loads(postResponse.text)["id"]
{{/hasKeyField}}
{{#schemaVars}}
{{^isKey}}
    self.assertEqual({{varName}}, {{schemaName}}.fromJSON(get{{schemaName}}(url, id).text).get_{{varName}}())
{{/isKey}}
{{/schemaVars}}

{{#schemaVars}}
{{#isKey}}
    {{varName}} = id
{{/isKey}}
{{^isKey}}
    {{varName}} = data2["{{varName}}"]
{{/isKey}}
{{/schemaVars}}

    putResponse = put{{schemaName}}(url, id, {{schemaName}}({{#schemaVars}}{{varName}}, {{/schemaVars}}))
    self.assertEqual(200, putResponse.status_code)
{{#schemaVars}}
{{^isKey}}
    self.assertEqual({{varName}}, {{schemaName}}.fromJSON(get{{schemaName}}(url, id).text).get_{{varName}}())
{{/isKey}}
{{/schemaVars}}

    deleteResponse = delete{{schemaName}}(url, id)
    self.assertEqual(200, deleteResponse.status_code)

    getAfterDeleteResponse = get{{schemaName}}(url, id)
    self.assertEqual(404, getAfterDeleteResponse.status_code)

{{/schemaRoute}}
{{/schema}}

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Url must be specified. Example: python tests.py http://localhost:3000")
    else:
        url = sys.argv[1]
        del sys.argv[1]
        unittest.main(argv = None)
