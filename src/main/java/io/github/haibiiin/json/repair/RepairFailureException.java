package io.github.haibiiin.json.repair;

public class RepairFailureException extends RuntimeException {

    public RepairFailureException() {
        super();
    }

    public RepairFailureException(String message) {
        super(message);
    }

    public RepairFailureException(String message, Throwable cause) {
        super(message, cause);
    }
}
