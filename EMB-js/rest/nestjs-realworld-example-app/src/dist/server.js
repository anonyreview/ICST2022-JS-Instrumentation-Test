"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const core_1 = require("@nestjs/core");
const app_module_1 = require("./app.module");
const swagger_1 = require("@nestjs/swagger");
const typeorm_1 = require("typeorm");
let app;
module.exports = {
    bootstrap: (port) => __awaiter(void 0, void 0, void 0, function* () {
        const appOptions = { cors: true };
        const connectionOptions = yield typeorm_1.getConnectionOptions();
        Object.assign(connectionOptions, { port: process.env.DB_PORT || 3306 });
        app = yield core_1.NestFactory.create(app_module_1.ApplicationModule, appOptions);
        app.setGlobalPrefix('api');
        const options = new swagger_1.DocumentBuilder()
            .setTitle('NestJS Realworld Example App')
            .setDescription('The Realworld API description')
            .setVersion('1.0')
            .setBasePath('api')
            .addBearerAuth()
            .build();
        const document = swagger_1.SwaggerModule.createDocument(app, options);
        swagger_1.SwaggerModule.setup('/docs', app, document);
        app.use('/swagger.json', (req, res) => {
            res.status(200);
            res.json(require('../swagger.json'));
        });
        return yield app.listen(port);
    }),
    stop: () => __awaiter(void 0, void 0, void 0, function* () {
        yield (app === null || app === void 0 ? void 0 : app.close());
    })
};
//# sourceMappingURL=server.js.map