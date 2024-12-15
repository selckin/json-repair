# json-repair : 大语言模型生成异常 JSON 应用层解决方案

[![Release](https://img.shields.io/github/release/HAibiiin/json-repair.svg?sort=semver)](https://github.com/HAibiiin/json-repair/releases/latest)
[![Maven Central](https://img.shields.io/maven-central/v/io.github.haibiiin/json-repair.svg)](https://search.maven.org/artifact/io.github.haibiiin/json-repair)
[![Apache 2.0 license](https://img.shields.io/badge/license-Apache%202.0-blue)](./LICENSE)
[![EN doc](https://img.shields.io/badge/document-English-blue.svg)](https://github.com/HAibiiin/json-repair/blob/main/README.md)

## json-repair 是什么？

**json-repair** 是一个在应用层为 LLMs（大语言模型）生成异常 JSON 提供修复的 Java 库。基于 **json-repair** 可以简单高效并准确的实现异常 JSON 的修复。

## 如何使用

要想使用 **json-repair**，只需将其作为依赖添加到你的 Java 项目中即可。 Maven 配置如下:

```xml
<dependency>
    <groupId>io.github.haibiiin</groupId>
    <artifactId>json-repair</artifactId>
    <version>0.1.0</version>
</dependency>
```

Gradle 配置如下：

```
implementation 'io.github.haibiiin:json-repair:0.1.0'
```

接下来你只需要实例化 **JSONRepair** 并调用 `handle()` 方法便可以完成异常 JSON 的修复。

```java
JSONRepair repair = new JSONRepair();
String correctJSON = repair.handle(mistakeJSON);
```

## 功能特性

你可以通过查看 [测试用例数据集](https://github.com/HAibiiin/json-repair/blob/main/src/test/resources/case/simple.xml) 了解当前 0.1.0 版本 **json-repair** 支持修补的所有 JSON 异常。

基于当前 0.1.0 版本的功能如下：

* 实现对 JSON 字符串基本修补功能
  * 修补缺少的右括号；
  * 修补缺少的右中括号；
  * 值为数组情况下多余逗号清理；
  * 值缺失以 null 填补。

## 开源许可

[Apache-2.0 license](https://github.com/HAibiiin/json-repair/blob/master/LICENSE).
