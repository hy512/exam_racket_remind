[TOC]

基于 web-server/http 实现一个 HTTP 服务.

已有 HTTP API 列表:
* /session/create
创建会话
* /calendar/list
日历列表

Header 自定义参数
* x-rem-token
/session/create, 返回, 会话 session-id.

实现功能:
- [x] 会话和缓存, 基于 memcached
- [x] DB, SQLite
- [x] 定时任务
- [ ] 文件日志

所需依赖
```
raco pkg install crontab
raco pkg install crontab-lib
raco pkg install functional
raco pkg install memcached
raco pkg install priority-queue
raco pkg install uuid
```

数据库文件
docs/sqlite3.sql

config.json, 配置项
config.json
```TypeScript
type cache_name_config = {
    // 过期时间, 秒
    expiration: number = 0
};

type config = {
    // 站点地址
    http: {
        port: number
        host: string
    }
    // 程序目录 
    home: string
    data_source: {
        type: "sqlite"
        // 数据库文件
        schema: ""
        sqlite: {
            // 绝对路径或相对
            filename: string
        }
    },
    // 缓存
    cache: {
        type: "memcached"
        memcached?: {
            ip: string
            port: number
        },
        config: cache_name_config,
        names: {
            [name: string]: cache_name_config
        }
    }
}
```