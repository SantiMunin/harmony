package com.prototype;

import static com.prototype.ServiceClient.Person;
import static com.prototype.ServiceClient.Task;

public class ServiceClientTest {

  private final static String A_STRING = "string";
  private final static Task TASK_1 = new Task(A_STRING, A_STRING);
  private final static Task TASK_2 = new Task(A_STRING, A_STRING);
  private final static Person PERSON = new Person(A_STRING, A_STRING, new ArrayList<Task>());

  private ServiceClient serviceClient = new ServiceClient("http://localhost:3000");
  
  @Test
  public void testCreatePerson() {
    ServerResponse<String> postResponse = serviceClient.postPerson(PERSON);
    assertEquals(200, postResponse.getStatusCode());
    String itemId = postResponse.getContent();
    assertEquals(PERSON.get_name(), serviceClient.getPerson(itemId).get_name());


  }


}


