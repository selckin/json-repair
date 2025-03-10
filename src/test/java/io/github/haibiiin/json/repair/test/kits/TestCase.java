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

public class TestCase {
    
    String anomaly;
    
    String correct;
    
    String reference;
    
    public TestCase(String anomaly, String correct, String reference) {
        this.anomaly = anomaly;
        this.correct = correct;
        this.reference = reference;
    }
    
    public String anomaly() {
        return anomaly;
    }
    
    public String correct() {
        return correct;
    }
    
    public String reference() {
        return reference;
    }
}
