from client import *
from hypothesis import *
from hypothesis.specifiers import * 
from hypothesis.strategies import *
import unittest
import copy
import sys

def is_valid_id_string(id_string):
  return len(id_string) > 0 \
         and len(id_string) < 20 \
         and all(map(lambda x: (ord(x) >= 65 and ord(x) <= 90) or (ord(x) >= 97 and ord(x) <= 122), id_string))

class ServiceTest(unittest.TestCase):

    def typesAre(self, t1, t2, name1, name2):
      return (t1 == name1 and t2 == name2) or (t1 == name2 and t2 == name1)

    def assertEqual(self, item1, item2):
        i1 = copy.deepcopy(item1)
        i2 = copy.deepcopy(item2)
        t1 = type(i1).__name__
        t2 = type(i2).__name__
        if type(i1) != type(i2):
            # some types can be treated as the same type
            if not (self.typesAre(t1, t2, "unicode", "str") or self.typesAre(t1, t2, "int", "float")):
                self.fail("Different types ("+ type(i1).__name__ + "," + type(i2).__name__ + ")")
        if (t1 == "float"):
           self.assertEquals_decimal(i1, i2)
        if isinstance(i1, dict):
            self.assertEquals_dict(i1, i2)
        elif isinstance(i1, list):
            self.assertEquals_list(i1, i2)
        else:  
            super(ServiceTest, self).assertEqual(i1, i2)

    def assertEquals_decimal(self, f1, f2):
      return abs(float(f1) - float(f2)) < 0.01
 
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

Settings.default.max_examples = 10000
Settings.default.timeout = 3
Settings.default.average_list_length = 3

{{#schema}}
{{schemaName}}Data = {
{{#schemaVars}}
{{#isEnum}}
  '{{varName}}': {{#isList}}[{{/isList}}sampled_from([{{#values}}"{{value}}", {{/values}}]){{#isList}}]{{/isList}},
{{/isEnum}}
{{^isEnum}}
{{^isKey}}
  '{{varName}}': {{&varType}}, 
{{/isKey}}
{{/isEnum}}
{{/schemaVars}}
}
{{/schema}}

{{#schema}}
{{#schemaRoute}}
class Test{{schemaName}}(ServiceTest):
  @given({{#schemaVars}}{{#isKey}}{{&varType}}, {{/isKey}}{{/schemaVars}}{{schemaName}}Data, {{schemaName}}Data{{#requiresAuth}}, lists(elements=one_of(integers(65, 90), integers(
        97, 122))).map(lambda l: map(chr, l)).map(lambda l: ''.join(l)){{/requiresAuth}})
  def test_insert_edit_delete(self, {{#hasKeyField}}id, {{/hasKeyField}}data, data2{{#requiresAuth}}, userData{{/requiresAuth}}):
{{#schemaVars}}
{{#isUserLogin}}
{{#isKey}}
    assume(is_valid_id_string(userData))
    id = userData
{{/isKey}}
{{^isKey}}
    {{varName}} = userData
{{/isKey}}
{{/isUserLogin}}
{{^isUserLogin}}
{{#isKey}}
    assume(is_valid_id_string(id))
    {{varName}} = id
{{/isKey}}
{{^isKey}}
    {{varName}} = data["{{varName}}"]
{{/isKey}}
{{/isUserLogin}}
{{/schemaVars}}
{{#requiresAuth}}
    assume(is_valid_id_string(userData))
    registerResponse = register(url, userData, userData)
    authResponse = login(url, userData, userData)
    self.assertEqual(authResponse.status_code, 200);
    token = json.loads(authResponse.text)['token']
{{/requiresAuth}}
{{#hasKeyField}}

    putResponse = put{{schemaName}}(url, id, {{schemaName}}({{#schemaVars}}{{^isUserLogin}}{{varName}} = {{varName}},{{/isUserLogin}} {{/schemaVars}}){{#requiresAuth}},token{{/requiresAuth}})
    self.assertEqual(200, putResponse.status_code)
{{/hasKeyField}}
{{^hasKeyField}}
    postResponse = post{{schemaName}}(url, {{schemaName}}({{#schemaVars}}{{^isUserLogin}}{{varName}} = {{varName}},{{/isUserLogin}} {{/schemaVars}}){{#requiresAuth}},token{{/requiresAuth}})
    self.assertEqual(201, postResponse.status_code)
    id = json.loads(postResponse.text)["id"]
{{/hasKeyField}}
{{#schemaVars}}
{{^isKey}}
    self.assertEqual({{varName}}, {{schemaName}}.fromJSON(get{{schemaName}}(url, id{{#requiresAuth}},token{{/requiresAuth}}).text).get_{{varName}}())
{{/isKey}}
{{/schemaVars}}

{{#schemaVars}}
{{#isUserLogin}}
    {{varName}} = userData
{{/isUserLogin}}
{{^isUserLogin}}
{{#isKey}}
    {{varName}} = id
{{/isKey}}
{{^isKey}}
    {{varName}} = data["{{varName}}"]
{{/isKey}}
{{/isUserLogin}}
{{/schemaVars}}

    putResponse = put{{schemaName}}(url, id, {{schemaName}}({{#schemaVars}}{{varName}}, {{/schemaVars}}){{#requiresAuth}},token{{/requiresAuth}})
    self.assertEqual(200, putResponse.status_code)
{{#schemaVars}}
{{^isKey}}
    self.assertEqual({{varName}}, {{schemaName}}.fromJSON(get{{schemaName}}(url, id{{#requiresAuth}},token{{/requiresAuth}}).text).get_{{varName}}())
{{/isKey}}
{{/schemaVars}}

    deleteResponse = delete{{schemaName}}(url, id{{#requiresAuth}},token{{/requiresAuth}})
    self.assertEqual(200, deleteResponse.status_code)

    getAfterDeleteResponse = get{{schemaName}}(url, id{{#requiresAuth}},token{{/requiresAuth}})
    self.assertEqual(404, getAfterDeleteResponse.status_code)

{{/schemaRoute}}
{{/schema}}

{{#requiresAuth}}
class LoginTest(ServiceTest):
  @given(lists(elements=one_of(integers(65, 90), integers(
        97, 122))).map(lambda l: map(chr, l)).map(lambda l: ''.join(l)))
  def login_test(self, userData):
    register = register(url, userData, userData)
    correctLoginResponse = login(url, userData, userData)
    assertEqual(200, correctLoginResponse.status_code)
    assertTrue(len(json.loads(correctLoginResponse.text)['token']) > 0)
    incorrectLoginResponse = login(url, userData, userData + "XYZ")
    assertEqual(401, incorrectLoginResponse.status_code)
{{/requiresAuth}}

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Url must be specified. Example: python tests.py http://localhost:3000")
    else:
        url = sys.argv[1]
        del sys.argv[1]
        unittest.main(argv = None)
