package io.github.haibiiin.json.repair;

public class OverstepTryTimesException extends RepairFailureException {

    public OverstepTryTimesException() {
        super();
    }

    public OverstepTryTimesException(String message) {
        super(message);
    }

    public OverstepTryTimesException(String message, Throwable cause) {
        super(message, cause);
    }
}
