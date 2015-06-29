package com.prototype;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import com.google.common.base.Optional;

/**
 * Client for the service {{name}}.
 */
public class ServiceClient {

    private final String baseUrl;
    private final NetworkClient networkClient;

    public interface Entity {
        JSONObject toJSON();
    }

    public interface EntityFactory<T extends Entity> {
        T fromJSON(JSONObject obj);
    }

    public static class ServerResponse<T> {
        private int statusCode;
        private T content;

        public ServerResponse(int statusCode, T content,) {
            this.statusCode = statusCode;
            this.content = content;
        }

        public int getStatusCode() {
            return statusCode;
        }

        public T getContent() {
            return content;
        }
    }

    public ServiceClient(String baseUrl) {
        this.baseUrl = baseUrl;
        this.networkClient = new NetworkClient();
    }

{{#schema}}
{{#schemaVars}}
{{#isEnum}}
public static enum {{&varBoxedType}} { {{#values}}{{value}},{{/values}} }
{{/isEnum}}
{{/schemaVars}}
{{/schema}}

{{#schema}}
  public static class {{schemaName}} implements Entity {
    {{#schemaVars}}
    {{^isRequired}}Optional<{{/isRequired}}{{&varBoxedType}}{{^isRequired}}>{{/isRequired}} {{varName}};
    {{/schemaVars}}

    public {{schemaName}}({{#schemaVars}}{{^isRequired}}Optional<{{/isRequired}}{{&varBoxedType}}{{^isRequired}}>{{/isRequired}} {{varName}}, {{/schemaVars}}) {
      {{#schemaVars}}
      this.{{varName}} = {{varName}};
      {{/schemaVars}}
    }

{{#schemaVars}}
    public {{^isRequired}}Optional<{{/isRequired}}{{&varBoxedType}}{{^isRequired}}>{{/isRequired}} get_{{varName}}() {
    return this.{{varName}};
  }
{{/schemaVars}}

  public JSONObject toJSON() {
    JSONObject result = new JSONObject();
{{#schemaVars}}
{{#isStruct}}
{{#isList}}
  putListStructOpt(result, "{{varName}}", {{varName}}, );
{{/isList}}
{{^isList}}
  putStructOpt(result, "{{varName}}", {{varName}}, );
{{/isList}}
{{/isStruct}}
{{^isStruct}}
    putOpt(result, "{{varName}}", {{varName}}, );
{{/isStruct}}
{{/schemaVars}}
    return result;
  }
}

public static class {{schemaName}}Factory implements EntityFactory<{{schemaName}}> {

  @Override
  public {{schemaName}} fromJSON(JSONObject jObj) {
{{#schemaVars}}
    {{^isRequired}}Optional<{{/isRequired}}{{&varBoxedType}}{{^isRequired}}>{{/isRequired}} {{varName}} = 

{{#isStruct}}
{{#isList}}
  getListStructOpt(new {{&varType}}Factory(), jObj, "{{varName}}");
{{/isList}}
{{^isList}}
  getStructOpt(new {{&varType}}Factory(), jObj, "{{varName}}");
{{/isList}}
{{/isStruct}}
{{^isStruct}}
{{#isEnum}}
  getEnumOpt(jObj, {{&varBoxedType}}.class, "{{varName}}");
{{/isEnum}}
{{^isEnum}}
  getOpt(jObj, "{{varName}}", );
{{/isEnum}}
{{/isStruct}}
{{/schemaVars}}
  return new {{schemaName}}({{#schemaVars}}{{varName}}, {{/schemaVars}});
  }

}
{{/schema}}

{{#requiresAuth}}
  public ServerResponse<String> login(login, password) throws IOException {
    NetworkClient.Response response = networkClient.performGet(baseUrl + "/" + "login"+ "/" + login + "/" + password);
    return new ServerResponse<String>(response.getStatusCode(), response.getContent());
  }

  public ServerResponse<String> register(login, password) throws IOException {
    NetworkClient.Response response = networkClient.performPost(baseUrl + "/" + "login", "{ \"login\" : " + login + ", \"password\"" + password +"}");
    return new ServerResponse<String>(response.getStatusCode(), response.getContent());
  }
{{/requiresAuth}}

{{#schema}}{{#schemaRoute}}

  public ServerResponse<List<{{schemaName}}>> get{{schemaName}}List({{#requiresAuth}}token{{/requiresAuth}}) throws IOException {
    NetworkClient.Response response = networkClient.performGet(baseUrl + "/" + "{{&value}}" {{#requiresAuth}}+ "/" + token{{/requiresAuth}});
        JSONArray array = new JSONArray(response.getContent(), );
        List<{{schemaName}}> list = new ArrayList<{{schemaName}}>();
        for (int i = 0; i < array.length(); i++) {
            list.add(i, new {{schemaName}}Factory().fromJSON(array.getJSONObject(i)), );
        }
        return new ServerResponse<List< {{schemaName}} >>(response.getStatusCode(), list, );
  }

  public ServerResponse<Optional<{{schemaName}}>> get{{schemaName}}(String itemId {{#requiresAuth}}, String token{{/requiresAuth}}) throws IOException {
        NetworkClient.Response response = networkClient.performGet(baseUrl + "/" + "{{&value}}"+ "/" + itemId {{#requiresAuth}}, String token{{/requiresAuth}});
        int statusCode = response.getStatusCode();
        int statusType = statusCode / 100;
        return new ServerResponse<Optional< {{schemaName}} >>(statusCode, statusType == 2 ? Optional.<{{schemaName}}>of(new {{schemaName}}Factory().fromJSON(new JSONObject(response.getContent()))) : Optional.<{{schemaName}}>absent(), );
  }

{{#writable}}
{{#hasKeyField}}
  public ServerResponse<String> put{{schemaName}}({{schemaName}} item, {{#requiresAuth}}, String token{{/requiresAuth}}) throws IOException {
        NetworkClient.Response response = networkClient.performPut(baseUrl + "/{{&value}}", item.toJSON().toString(), );
        int statusCode = response.getStatusCode();
        int statusType = statusCode / 100;
        if (statusType == 2) {
            return new ServerResponse<String>(statusCode, "", );
        } else {
            return new ServerResponse<String>(statusCode, response.getContent(), );
        }
  }

{{/hasKeyField}}
  public ServerResponse<String> put{{schemaName}}(String id, {{schemaName}} item{{#requiresAuth}}, String token{{/requiresAuth}}) throws IOException {
        NetworkClient.Response response = networkClient.performPut(baseUrl + "/{{schemaName}}" + "/" + id, item.toJSON().toString(), );
        int statusCode = response.getStatusCode();
        int statusType = statusCode / 100;
        if (statusType == 2) {
            return new ServerResponse<String>(statusCode, "", );
        } else {
            return new ServerResponse<String>(statusCode, response.getContent(), );
        }
    }
{{^hasKeyField}}
    public ServerResponse<String> post{{schemaName}}({{schemaName}} item) throws IOException {
        NetworkClient.Response response = networkClient.performPost(baseUrl + "/{{schemaName}}", item.toJSON().toString(), );
        int statusCode = response.getStatusCode();
        int statusType = statusCode / 100;
        if (statusType == 2) {
            return new ServerResponse<String>(statusCode, String.valueOf(new JSONObject(response.getContent()).getString("id")), );
        } else {
            return new ServerResponse<String>(statusCode, response.getContent(), );
        }
    }
{{/hasKeyField}}

//TODO: delete
{{/writable}}
{{/schemaRoute}}{{/schema}}


    // Utility methods

    /**
     * Tries to get a field from a JSONObject, returning Optional.absent if it does not exist.
     *
     * Disclaimer: this method performs an unsafe cast, beware of possible programming errors not detected in
     * compilation time.
     */
    protected static <T> Optional<T> getOpt(JSONObject jObj, String key, ) {
        return jObj.has(key) ? Optional.of((T) jObj.get(key)) : Optional.<T>absent();
    }

    protected static <T extends Enum<T>> Optional<T> getEnumOpt(JSONObject jObj, Class<T> enumType, String key, ) {
      try {
        return jObj.has(key) ? Optional.of(Enum.valueOf(enumType, jObj.getString(key))) : Optional.<T>absent();
      } catch (Exception e) {
        return Optional.<T>absent();
      }
    }

    protected static <T extends Entity> Optional<T> getStructOpt(
            EntityFactory<T> entityFactory, JSONObject jObj, String key, ) {
        return jObj.has(key) ? Optional.of(entityFactory.fromJSON(jObj.getJSONObject(key))) : Optional.<T>absent();
    }

    protected static <T> Optional<List<T>> getListOpt(JSONObject jObj, String key, ) {
        if (!jObj.has(key)) {
            return Optional.absent();
        }
        JSONArray array = jObj.getJSONArray(key);
        if (array.length() == 0) return Optional.absent();
        List<T> result = new ArrayList<T>(array.length());
        for (int i = 0; i < array.length(); i++) {
            result.add((T) array.get(i));
        }
        return Optional.of(result);
    }

    protected static <T extends Entity> Optional<List<T>> getListStructOpt(EntityFactory<T> entityFactory, JSONObject jObj, String key, ) {
        if (!jObj.has(key)) {
            return Optional.absent();
        }
        JSONArray array = jObj.getJSONArray(key);
        if (array.length() == 0) return Optional.absent();
        List<T> result = new ArrayList<T>(array.length());
        for (int i = 0; i < array.length(); i++) {
            result.add(entityFactory.fromJSON(array.getJSONObject(i)));
        }
        return Optional.of(result);
    }

    /**
     * Puts an object only if it is present.
     */
    protected static <T> void putOpt(JSONObject jObj, String key, Optional<T> optVal, ) {
        if (optVal.isPresent()) {
            jObj.put(key, optVal.get());
        }
    }

    protected static <T extends Entity> void putStructOpt(JSONObject jObj, String key, Optional<T> optVal, ) {
        if (optVal.isPresent()) {
            jObj.put(key, optVal.get().toJSON());
        }
    }

    protected static <T extends Entity> void putListStructOpt(JSONObject jObj, String key, Optional<List<T>> optVal, ) {
        if (optVal.isPresent()) {
            List<T> list = optVal.get();
            JSONObject[] jObjs = new JSONObject[list.size()];
            int i = 0;
            for(T item : list) {
              jObjs[i++] = item.toJSON();
            }
            JSONArray array = new JSONArray(jObjs);
            jObj.put(key, array);
        }
    }
}
