import requests, json


class PrivateMessage:
    def __init__(self, author=None, to=None, timestamp=None, content=None, ):
        self._info = {}
        self._info["author"] = author
        self._info["to"] = to
        self._info["timestamp"] = timestamp
        self._info["content"] = content

    @staticmethod
    def fromDict(dict):
        return PrivateMessage(dict["author"], dict["to"], dict["timestamp"],
                              dict["content"], )

    def toDict(self):
        return self._info

    @staticmethod
    def fromJSON(text):
        return PrivateMessage.fromDict(json.loads(text))

    def toJSON(self):
        return json.dumps(self._info)

    def get_author(self):
        return self._info["author"]

    def set_author(self, author):
        self._info["author"] = author

    def get_to(self):
        return self._info["to"]

    def set_to(self, to):
        self._info["to"] = to

    def get_timestamp(self):
        return self._info["timestamp"]

    def set_timestamp(self, timestamp):
        self._info["timestamp"] = timestamp

    def get_content(self):
        return self._info["content"]

    def set_content(self, content):
        self._info["content"] = content


class TopicList:
    def __init__(self, name=None, accounts=None, ):
        self._info = {}
        self._info["name"] = name
        self._info["accounts"] = accounts

    @staticmethod
    def fromDict(dict):
        return TopicList(dict["name"], dict["accounts"], )

    def toDict(self):
        return self._info

    @staticmethod
    def fromJSON(text):
        return TopicList.fromDict(json.loads(text))

    def toJSON(self):
        return json.dumps(self._info)

    def get_name(self):
        return self._info["name"]

    def set_name(self, name):
        self._info["name"] = name

    def get_accounts(self):
        return self._info["accounts"]

    def set_accounts(self, accounts):
        self._info["accounts"] = accounts


class Tweet:
    def __init__(self, author=None, timestamp=None, content=None, kind=None, ):
        self._info = {}
        self._info["author"] = author
        self._info["timestamp"] = timestamp
        self._info["content"] = content
        self._info["kind"] = kind

    @staticmethod
    def fromDict(dict):
        return Tweet(dict["author"], dict["timestamp"], dict["content"],
                     dict["kind"], )

    def toDict(self):
        return self._info

    @staticmethod
    def fromJSON(text):
        return Tweet.fromDict(json.loads(text))

    def toJSON(self):
        return json.dumps(self._info)

    def get_author(self):
        return self._info["author"]

    def set_author(self, author):
        self._info["author"] = author

    def get_timestamp(self):
        return self._info["timestamp"]

    def set_timestamp(self, timestamp):
        self._info["timestamp"] = timestamp

    def get_content(self):
        return self._info["content"]

    def set_content(self, content):
        self._info["content"] = content

    def get_kind(self):
        return self._info["kind"]

    def set_kind(self, kind):
        self._info["kind"] = kind


def login(url, login, password):
    return requests.post(url + "/u_auth" + "/" + login + "/" + password)


def register(url, login, password):
    return requests.post(
        url + "/u_auth/register",
        data=json.dumps({'login': login,
                         'password': password}),
        headers={'content-type': 'application/json'})


def getPrivateMessage_list(url, token):
    return requests.get(url + "/message" + "/" + token)


def getPrivateMessage(url, item_id, token):
    return requests.get(url + "/message" + "/" + item_id + "/" + token)


def putPrivateMessage(url, item_id, item, token):
    return requests.put(url + "/message" + "/" + item_id + "/" + token,
                        data=item.toJSON(),
                        headers={'content-type': 'application/json'})


def postPrivateMessage(url, item, token):
    return requests.post(url + "/message" + "/" + token,
                         data=item.toJSON(),
                         headers={'content-type': 'application/json'})


def deletePrivateMessage(url, item_id, token):
    return requests.delete(url + "/message" + "/" + item_id + "/" + token)


def getTopicList_list(url, token):
    return requests.get(url + "/topicList" + "/" + token)


def getTopicList(url, item_id, token):
    return requests.get(url + "/topicList" + "/" + item_id + "/" + token)


def putTopicList(url, item, token):
    return requests.put(
        url + "/topicList" + "/" + item.get_name() + "/" + token,
        data=item.toJSON(),
        headers={'content-type': 'application/json'})


def putTopicList(url, item_id, item, token):
    return requests.put(url + "/topicList" + "/" + item_id + "/" + token,
                        data=item.toJSON(),
                        headers={'content-type': 'application/json'})


def deleteTopicList(url, item_id, token):
    return requests.delete(url + "/topicList" + "/" + item_id + "/" + token)


def getTweet_list(url, token):
    return requests.get(url + "/tweet" + "/" + token)


def getTweet(url, item_id, token):
    return requests.get(url + "/tweet" + "/" + item_id + "/" + token)


def putTweet(url, item_id, item, token):
    return requests.put(url + "/tweet" + "/" + item_id + "/" + token,
                        data=item.toJSON(),
                        headers={'content-type': 'application/json'})


def postTweet(url, item, token):
    return requests.post(url + "/tweet" + "/" + token,
                         data=item.toJSON(),
                         headers={'content-type': 'application/json'})


def deleteTweet(url, item_id, token):
    return requests.delete(url + "/tweet" + "/" + item_id + "/" + token)
