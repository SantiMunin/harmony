package com.prototype;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

/**
 * Handles network operations (GET, POST, PUT, DELETE...).
 */
public class NetworkClient {

    /**
     * Response of a network call (status code, response body).
     */
    public static class Response {
        private int statusCode;
        private String content;

        public Response(int statusCode, String content) {
            this.content = content;
            this.statusCode = statusCode;
        }

        public int getStatusCode() {
            return statusCode;
        }

        public String getContent() {
            return content;
        }
    }

    /**
     * Performs a get operation, returning code and response body/error.
     */
    public Response performGet(String url) throws IOException {
        HttpURLConnection httpConnection = (HttpURLConnection) new URL(url).openConnection();
        return new Response(httpConnection.getResponseCode(), getContent(httpConnection));
    }

    /**
     * Performs a post operation, returning code and response body/error.
     */
    public Response performPost(String url, String body) throws IOException {
        return performOperationWithBody(url, body, "POST");
    }

    /**
     * Performs a put operation, returning code and response body/error.
     */
    public Response performPut(String url, String body) throws IOException {
        return performOperationWithBody(url, body, "PUT");
    }

    private Response performOperationWithBody(String url, String body, String method) throws IOException {
        HttpURLConnection httpConnection = (HttpURLConnection) new URL(url).openConnection();
        httpConnection.setRequestMethod(method);
        httpConnection.setRequestProperty("Content-Type", "application/json");
        httpConnection.setDoOutput(true);
        httpConnection.setDoInput(true);
        DataOutputStream wr = new DataOutputStream(httpConnection.getOutputStream());
        wr.writeBytes(body);
        wr.flush();
        wr.close();
        return new Response(httpConnection.getResponseCode(), getContent(httpConnection));
    }

    private String getContent(HttpURLConnection conn) throws IOException {
        int errorCategory = conn.getResponseCode() / 100;

        BufferedReader in = new BufferedReader(
            new InputStreamReader(
                errorCategory == 4 || errorCategory == 5 ? conn.getErrorStream() : conn.getInputStream()));
        try {
            String inputLine;
            StringBuilder response = new StringBuilder();

            while ((inputLine = in.readLine()) != null) {
                response.append(inputLine);
            }
            return response.toString();
        } finally {
          in.close();
        }
    }
}
