/*
 * Copyright 2024 HAibiiin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.github.haibiiin.json.repair.test.kits;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.security.AnyTypePermission;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.ArgumentsProvider;
import org.junit.jupiter.params.support.AnnotationConsumer;

public class TestCaseArgumentsProvider implements ArgumentsProvider, AnnotationConsumer<TestCaseSource> {
    
    String filePath;
    
    FixerStrategy dataType;
    
    @Override
    public Stream<? extends Arguments> provideArguments(ExtensionContext extensionContext) {
        XStream xStream = new XStream();
        xStream.alias("test-cases", ArrayList.class);
        xStream.alias("test-case", TestCase.class);
        xStream.addPermission(AnyTypePermission.ANY);
        xStream.setClassLoader(this.getClass().getClassLoader());
        List<TestCase> testCaseList = (List<TestCase>) xStream.fromXML(this.getClass().getResource(filePath));
        switch (dataType) {
            case CORRECT:
                return testCaseList.stream().map((testCase -> Arguments.of(testCase.anomaly, testCase.correct, testCase.reference)));
            case SIMPLE:
            default:
                return testCaseList.stream().map((testCase -> Arguments.of(testCase.anomaly, testCase.correct)));
        }
    }
    
    @Override
    public void accept(TestCaseSource testCaseSource) {
        this.filePath = testCaseSource.path();
        this.dataType = testCaseSource.type();
    }
}
