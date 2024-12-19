package io.github.haibiiin.json.repair;

import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;
import org.openjdk.jmh.results.format.ResultFormatType;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

import java.util.concurrent.TimeUnit;

@State(Scope.Thread)
@BenchmarkMode(Mode.All)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
public class BenchmarkTests {

    @Param({
            "{\"f\":\"v\", \"f2\":\"v2\"",
            "{\"f\":\"v\", \"a\":[1",
            "{\"f\":\"v\", \"a\":[1,2], \"o1\":{\"f1\":\"v1\"}, ",
            "\"f\":\"v\", \"a\":[1,2], \"o1\":{\"f1\":\"v1\"}",
            "f:v"
    })
    String anomalyJSON;

    @Benchmark
    public void testSimpleRepairStrategy(Blackhole blackhole) {
        JSONRepair repair = new JSONRepair();
        blackhole.consume(repair.handle(anomalyJSON));
    }

    public static void main(String[] args) throws RunnerException {
        Options opt = new OptionsBuilder()
                .include(BenchmarkTests.class.getSimpleName())
                .result("./reports/benchmark/benchmark_0.1.0.json")
                .resultFormat(ResultFormatType.JSON)
                .build();
        new Runner(opt).run();
    }

}
