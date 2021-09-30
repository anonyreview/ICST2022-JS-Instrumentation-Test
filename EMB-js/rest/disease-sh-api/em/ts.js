const Redis = require('ioredis');
const { DockerComposeEnvironment , Wait } = require("testcontainers");
let exposedDbPort;
let host;
let test_container;
let redis;
async function connect(){
    const dbPort = 50000;
    const environment = await new DockerComposeEnvironment(__dirname, "test-redis-db.yml")
        .withWaitStrategy("redis_1", Wait.forLogMessage("Ready to accept connections"))
        .up();
    test_container = await environment.getContainer("redis_1");
    exposedDbPort = test_container.getMappedPort(dbPort);
    host = test_container.getHost();

    console.log(host + " "+exposedDbPort);
}

connect().then(async ()=>{

    console.log("connecting... to "+ host + " "+exposedDbPort);
    redis = new Redis(host, {
        password: '',
        port: exposedDbPort
    });


    redis.keys("*", function (err, result) {
        if (err) {
            console.error(err);
        } else {
            console.log(result);
        }
    });

    await redis.flushall();

    redis.keys("*", async function (err, result) {
        if (err) {
            console.error(err);
        } else {
            console.log(result);
        }
    });

    await redis.disconnect();
    await test_container.stop();
});



