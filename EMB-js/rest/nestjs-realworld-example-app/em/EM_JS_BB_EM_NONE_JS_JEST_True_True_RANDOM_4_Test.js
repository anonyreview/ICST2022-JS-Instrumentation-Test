const superagent = require("superagent");
const EM = require("evomaster-client-js").EMTestUtils;
const AppController = require("./app-driver.js");




/**
 * This file was automatically generated by EvoMaster on 2021-09-06T18:57:35.190+02:00[Europe/Berlin]
 * 
 * The generated test suite contains 34 tests
 * 
 * Covered targets: 75
 * 
 * Used time: 0h 24m 22s
 * 
 * Needed budget for current results: 71%
 * 
 * 
 */


const controller = new AppController();
let baseUrlOfSut;


beforeAll( async () => {
    await controller.setupForGeneratedTest();
    baseUrlOfSut = await controller.startSut();
    expect(baseUrlOfSut).toBeTruthy();
});


afterAll( async () => {
    await controller.stopSut();
});


beforeEach(async () =>  {
    await controller.resetStateOfSUT();
});









test("test_0_with500", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .delete(baseUrlOfSut + "/api/articles/ZblT/comments/239").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .ok(res => res.status);
    
    expect(res_0.status).toBe(500);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(500.0);
    expect(res_0.body.message).toBe("Internal server error");
});


test("test_1_with500", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .post(baseUrlOfSut + "/api/articles/bbwWRLv/favorite").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .ok(res => res.status);
    
    expect(res_0.status).toBe(500);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(500.0);
    expect(res_0.body.message).toBe("Internal server error");
});


test("test_2_with500", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .post(baseUrlOfSut + "/api/articles/suWr7PuU/comments").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"comment\": { " + 
                " \"body\": \"suWr7PuU\" " + 
                " } " + 
                " } ")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(500);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(500.0);
    expect(res_0.body.message).toBe("Internal server error");
});


test("test_3_with500", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .delete(baseUrlOfSut + "/api/profiles/FX17/follow").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .ok(res => res.status);
    
    expect(res_0.status).toBe(500);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(500.0);
    expect(res_0.body.message).toBe("Internal server error");
});


test("test_4_with500", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .put(baseUrlOfSut + "/api/articles/raF0xaPMuO467").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"article\": {} " + 
                " } ")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(500);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(500.0);
    expect(res_0.body.message).toBe("Internal server error");
});


test("test_5_with500", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .get(baseUrlOfSut + "/api/articles?" + 
                "favorited=&" + 
                "offset=629").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .ok(res => res.status);
    
    expect(res_0.status).toBe(500);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(500.0);
    expect(res_0.body.message).toBe("Internal server error");
});


test("test_6_with500", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .get(baseUrlOfSut + "/api/articles/d29qHji_qYxD8/comments").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .ok(res => res.status);
    
    expect(res_0.status).toBe(500);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(500.0);
    expect(res_0.body.message).toBe("Internal server error");
});


test("test_7_with500", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .post(baseUrlOfSut + "/api/profiles/IhXVHAPOf7/follow").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .ok(res => res.status);
    
    expect(res_0.status).toBe(500);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(500.0);
    expect(res_0.body.message).toBe("Internal server error");
});


test("test_8_with500", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .delete(baseUrlOfSut + "/api/articles/wSozwbA043/favorite").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .ok(res => res.status);
    
    expect(res_0.status).toBe(500);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(500.0);
    expect(res_0.body.message).toBe("Internal server error");
});


test("test_9", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .get(baseUrlOfSut + "/api/profiles/m").set('Accept', "*/*")
            .set("Authorization", token_foo) // foo-auth
            .ok(res => res.status);
    
    expect(res_0.status).toBe(200);
    expect(res_0.body===null || res_0.body===undefined || res_0.body==="" || Object.keys(res_0.body).length === 0).toBe(true);
});


test("test_10", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .get(baseUrlOfSut + "/api/user").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .ok(res => res.status);
    
    expect(res_0.status).toBe(200);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.user.username).toBe("foo");
    expect(res_0.body.user.email).toBe("foo@foo.foo");
    expect(res_0.body.user.bio).toBe("");
    expect(res_0.body.user.token).toBe("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6MSwidXNlcm5hbWUiOiJmb28iLCJlbWFpbCI6ImZvb0Bmb28uZm9vIiwiZXhwIjoxNjM2MTMzNjAxLjQyMywiaWF0IjoxNjMwOTQ2MDAxfQ.ETzatZqrPefKtPa0z6hE81LW0e-WFh1mrdcVUCVlKSY");
    expect(res_0.body.user.image).toBe("");
});


test("test_11", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .delete(baseUrlOfSut + "/api/articles/QX4Peha").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .ok(res => res.status);
    
    expect(res_0.status).toBe(200);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.raw.fieldCount).toBe(0.0);
    expect(res_0.body.raw.affectedRows).toBe(0.0);
    expect(res_0.body.raw.insertId).toBe(0.0);
    expect(res_0.body.raw.serverStatus).toBe(34.0);
    expect(res_0.body.raw.warningCount).toBe(0.0);
    expect(res_0.body.raw.message).toBe("");
    expect(res_0.body.raw.protocol41).toBe(true);
    expect(res_0.body.raw.changedRows).toBe(0.0);
    expect(res_0.body.affected).toBe(0.0);
});


test("test_12", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .get(baseUrlOfSut + "/api/tags").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .ok(res => res.status);
    
    expect(res_0.status).toBe(200);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.length).toBe(0);
});


test("test_13", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .get(baseUrlOfSut + "/api/articles/feed?limit=265").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .ok(res => res.status);
    
    expect(res_0.status).toBe(200);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.articles.length).toBe(0);
    expect(res_0.body.articlesCount).toBe(0.0);
});


test("test_14", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .get(baseUrlOfSut + "/api/articles?tag=WE8gujZHiQeWCdv").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .ok(res => res.status);
    
    expect(res_0.status).toBe(200);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.articles.length).toBe(0);
    expect(res_0.body.articlesCount).toBe(0.0);
});


test("test_15", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .put(baseUrlOfSut + "/api/user").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"token\": \"jtOFO\", " + 
                " \"username\": \"y9q3\", " + 
                " \"bio\": \"UZSrAh\" " + 
                " } " + 
                " } ")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(200);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.username).toBe("y9q3");
    expect(res_0.body.email).toBe("foo@foo.foo");
    expect(res_0.body.bio).toBe("UZSrAh");
    expect(res_0.body.image).toBe("");
    expect(res_0.body.token).toBe("jtOFO");
});


test("test_16", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .get(baseUrlOfSut + "/api/articles/5K2").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .ok(res => res.status);
    
    expect(res_0.status).toBe(200);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(Object.keys(res_0.body).length).toBe(0);
});


test("test_17", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .put(baseUrlOfSut + "/api/user").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"\", " + 
                " \"username\": \"i\", " + 
                " \"bio\": \"\", " + 
                " \"image\": \"Txr4REC\" " + 
                " } " + 
                " } ")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(200);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.username).toBe("i");
    expect(res_0.body.email).toBe("");
    expect(res_0.body.bio).toBe("");
    expect(res_0.body.image).toBe("Txr4REC");
    
    const res_1 = await superagent
            .post(baseUrlOfSut + "/api/profiles/k07VhfDD6E8uFKv4/follow").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .ok(res => res.status);
    
    expect(res_1.status).toBe(400);
    expect(res_1.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_1.body.statusCode).toBe(400.0);
    expect(res_1.body.message).toBe("Follower email and username not provided.");
});


test("test_18", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .post(baseUrlOfSut + "/api/articles").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"article\": { " + 
                " \"title\": \"ovaMk68bSVR_7997\", " + 
                " \"description\": \"LIz5vqzPjI\", " + 
                " \"body\": \"KrfcP3q\", " + 
                " \"tagList\": [ " + 
                " \"MHv2\", " + 
                " \"A2QnM\", " + 
                " \"H\" " + 
                " ] " + 
                " } " + 
                " } ")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(201);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.title).toBe("ovaMk68bSVR_7997");
    expect(res_0.body.description).toBe("LIz5vqzPjI");
    expect(res_0.body.slug).toBe("ovamk68bsvr_7997-mr1liw");
    expect(res_0.body.tagList.length).toBe(3);
    expect(res_0.body.tagList[0]).toBe("MHv2");
    expect(res_0.body.tagList[1]).toBe("A2QnM");
    expect(res_0.body.tagList[2]).toBe("H");
    expect(res_0.body.comments.length).toBe(0);
    expect(res_0.body.body).toBe("");
    expect(res_0.body.created).toBe("2021-09-06T14:33:23.000Z");
    expect(res_0.body.updated).toBe("2021-09-06T14:33:23.000Z");
    expect(res_0.body.favoriteCount).toBe(0.0);
});


test("test_19", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .post(baseUrlOfSut + "/api/users/login").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"W6S\", " + 
                " \"password\": \"\" " + 
                " } " + 
                " } ")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(400);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.message).toBe("Input data validation failed");
    expect(res_0.body.errors.passwordisNotEmpty).toBe("password should not be empty");
});


test("test_20", async () => {
    
    const res_0 = await superagent
            .post(baseUrlOfSut + "/api/articles/tia4kHqWdEv/favorite").set('Accept', "application/json")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(401);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(401.0);
    expect(res_0.body.message).toBe("Not authorized.");
});


test("test_21", async () => {
    
    const res_0 = await superagent
            .post(baseUrlOfSut + "/api/users/login").set('Accept', "application/json")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"R\", " + 
                " \"password\": \"3dYzCKzoS6y4l\" " + 
                " } " + 
                " } ")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(401);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.errors.User).toBe(" not found");
});


test("test_22", async () => {
    
    const res_0 = await superagent
            .delete(baseUrlOfSut + "/api/articles/PWqfVu/favorite").set('Accept', "application/json")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(401);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(401.0);
    expect(res_0.body.message).toBe("Not authorized.");
});


test("test_23", async () => {
    
    const res_0 = await superagent
            .post(baseUrlOfSut + "/api/articles/SeA6Dv/comments").set('Accept', "application/json")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"comment\": { " + 
                " \"body\": \"SeA6Dv\" " + 
                " } " + 
                " } ")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(401);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(401.0);
    expect(res_0.body.message).toBe("Not authorized.");
});


test("test_24", async () => {
    
    const res_0 = await superagent
            .delete(baseUrlOfSut + "/api/profiles/iE3YJIVTmG9Kw/follow").set('Accept', "application/json")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(401);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(401.0);
    expect(res_0.body.message).toBe("Not authorized.");
});


test("test_25", async () => {
    
    const res_0 = await superagent
            .put(baseUrlOfSut + "/api/user").set('Accept', "application/json")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"DSqR2qWjHM\", " + 
                " \"token\": \"IlYc7ijGRd\", " + 
                " \"bio\": \"BVq6fiIxJ\" " + 
                " } " + 
                " } ")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(401);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(401.0);
    expect(res_0.body.message).toBe("Not authorized.");
});


test("test_26", async () => {
    
    const res_0 = await superagent
            .delete(baseUrlOfSut + "/api/articles/QpY").set('Accept', "application/json")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(401);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(401.0);
    expect(res_0.body.message).toBe("Not authorized.");
});


test("test_27", async () => {
    
    const res_0 = await superagent
            .post(baseUrlOfSut + "/api/articles").set('Accept', "application/json")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"article\": { " + 
                " \"title\": \"AOgCj_arfSY\", " + 
                " \"description\": \"zv\", " + 
                " \"body\": \"E4kqOWm9L0\", " + 
                " \"tagList\": [ " + 
                " \"\", " + 
                " \"j\" " + 
                " ] " + 
                " } " + 
                " } ")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(401);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(401.0);
    expect(res_0.body.message).toBe("Not authorized.");
});


test("test_28", async () => {
    
    const res_0 = await superagent
            .delete(baseUrlOfSut + "/api/articles/96B0B_g_d1SrG/comments/769").set('Accept', "application/json")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(401);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(401.0);
    expect(res_0.body.message).toBe("Not authorized.");
});


test("test_29", async () => {
    
    const res_0 = await superagent
            .get(baseUrlOfSut + "/api/articles/feed?" + 
                "limit=135&" + 
                "offset=607").set('Accept', "application/json")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(401);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(401.0);
    expect(res_0.body.message).toBe("Not authorized.");
});


test("test_30", async () => {
    
    const res_0 = await superagent
            .put(baseUrlOfSut + "/api/articles/w0").set('Accept', "application/json")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"article\": {} " + 
                " } ")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(401);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(401.0);
    expect(res_0.body.message).toBe("Not authorized.");
});


test("test_31", async () => {
    
    const res_0 = await superagent
            .post(baseUrlOfSut + "/api/profiles/4NBPW/follow").set('Accept', "application/json")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(401);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(401.0);
    expect(res_0.body.message).toBe("Not authorized.");
});


test("test_32", async () => {
    
    const res_0 = await superagent
            .get(baseUrlOfSut + "/api/user").set('Accept', "application/json")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(401);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(401.0);
    expect(res_0.body.message).toBe("Not authorized.");
});


test("test_33", async () => {
    
    let token_foo = "token ";
    await superagent
            .post(baseUrlOfSut + "/api/users/login")
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"email\": \"foo@foo.foo\", " + 
                " \"password\": \"foofoo\" " + 
                " } " + 
                " } ")
            .then(res => {token_foo += res.body.user.token;});
            
    
    const res_0 = await superagent
            .post(baseUrlOfSut + "/api/user").set('Accept', "application/json")
            .set("Authorization", token_foo) // foo-auth
            .set('Content-Type','application/json')
            .send(" { " + 
                " \"user\": { " + 
                " \"username\": \"Bd\", " + 
                " \"email\": \"TjvR5NQibR4j\", " + 
                " \"password\": \"tE4W52E\" " + 
                " } " + 
                " } ")
            .ok(res => res.status);
    
    expect(res_0.status).toBe(404);
    expect(res_0.header["content-type"].startsWith("application/json")).toBe(true);
    expect(res_0.body.statusCode).toBe(404.0);
    expect(res_0.body.message).toBe("Cannot POST /api/user");
    expect(res_0.body.error).toBe("Not Found");
});
