/*
 * Copyright (c) 2021 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.communicator.other.genericserver.utils;

import java.util.HashMap;
import java.util.Map;

/**
 * Util class to store Http status code and its description
 */
public class HttpStatus {
	/**
	 * Map to store status code and its description
	 */
	private static final Map<Integer, String> STATUS_CODE_MAP = new HashMap<>();

	static {
		// 1xx cases
		STATUS_CODE_MAP.put(100, "Continue");
		STATUS_CODE_MAP.put(101, "Switching Protocol");
		STATUS_CODE_MAP.put(102, "Processing (WebDAV)");
		STATUS_CODE_MAP.put(103, "Early Hints");
		// 2xx case
		STATUS_CODE_MAP.put(200, "OK");
		STATUS_CODE_MAP.put(201, "Created");
		STATUS_CODE_MAP.put(202, "Accepted");
		STATUS_CODE_MAP.put(203, "Non-Authoritative Information");
		STATUS_CODE_MAP.put(204, "No Content");
		STATUS_CODE_MAP.put(205, "Reset Content");
		STATUS_CODE_MAP.put(206, "Partial Content");
		STATUS_CODE_MAP.put(207, "Multi-Status (WebDAV)");
		STATUS_CODE_MAP.put(208, "Already Reported (WebDAV)");
		STATUS_CODE_MAP.put(226, "IM Used (HTTP Delta encoding)");
		// 3xx cases
		STATUS_CODE_MAP.put(300, "Multiple Choice");
		STATUS_CODE_MAP.put(301, "Moved Permanently");
		STATUS_CODE_MAP.put(302, "Found");
		STATUS_CODE_MAP.put(303, "See Other");
		STATUS_CODE_MAP.put(304, "Not Modified");
		STATUS_CODE_MAP.put(305, "Use Proxy");
		STATUS_CODE_MAP.put(306, "unused");
		STATUS_CODE_MAP.put(307, "Temporary Redirect");
		STATUS_CODE_MAP.put(308, "Permanent Redirect");
		// 4xx cases
		STATUS_CODE_MAP.put(400, "Bad Request");
		STATUS_CODE_MAP.put(401, "Unauthorized");
		STATUS_CODE_MAP.put(402, "Payment Required");
		STATUS_CODE_MAP.put(403, "Forbidden");
		STATUS_CODE_MAP.put(404, "Not Found");
		STATUS_CODE_MAP.put(405, "Method Not Allowed");
		STATUS_CODE_MAP.put(406, "Not Acceptable");
		STATUS_CODE_MAP.put(407, "Proxy Authentication Required");
		STATUS_CODE_MAP.put(408, "Request Timeout");
		STATUS_CODE_MAP.put(409, "Conflict");
		STATUS_CODE_MAP.put(410, "Gone");
		STATUS_CODE_MAP.put(411, "Length Required");
		STATUS_CODE_MAP.put(412, "Precondition Failed");
		STATUS_CODE_MAP.put(413, "Payload Too Large");
		STATUS_CODE_MAP.put(414, "URI Too Long");
		STATUS_CODE_MAP.put(415, "Unsupported Media Type");
		STATUS_CODE_MAP.put(416, "Range Not Satisfiable");
		STATUS_CODE_MAP.put(417, "Expectation Failed");
		STATUS_CODE_MAP.put(418, "I'm a teapot");
		STATUS_CODE_MAP.put(421, "Misdirected Request");
		STATUS_CODE_MAP.put(422, "Unprocessable Entity (WebDAV)");
		STATUS_CODE_MAP.put(423, "Locked (WebDAV)");
		STATUS_CODE_MAP.put(424, "Failed Dependency (WebDAV)");
		STATUS_CODE_MAP.put(425, "Too Early");
		STATUS_CODE_MAP.put(426, "Upgrade Required");
		STATUS_CODE_MAP.put(428, "Precondition Required");
		STATUS_CODE_MAP.put(429, "Too Many Requests");
		STATUS_CODE_MAP.put(431, "Request Header Fields Too Large");
		STATUS_CODE_MAP.put(451, "Unavailable For Legal Reasons");
		// 5xx cases
		STATUS_CODE_MAP.put(500, "Internal Server Error");
		STATUS_CODE_MAP.put(501, "Not Implemented");
		STATUS_CODE_MAP.put(502, "Bad Gateway");
		STATUS_CODE_MAP.put(503, "Service Unavailable");
		STATUS_CODE_MAP.put(504, "Gateway Timeout");
		STATUS_CODE_MAP.put(505, "HTTP Version Not Supported");
		STATUS_CODE_MAP.put(506, "Variant Also Negotiates");
		STATUS_CODE_MAP.put(507, "Insufficient Storage (WebDAV)");
		STATUS_CODE_MAP.put(508, "Loop Detected (WebDAV)");
		STATUS_CODE_MAP.put(510, "Not Extended");
		STATUS_CODE_MAP.put(511, "Network Authentication Required");
	}

	/**
	 * Get status code description
	 *
	 * @return String This returns the <description> of status code
	 */
	public static String getDescription(int statusCode) {
		return STATUS_CODE_MAP.get(statusCode);
	}

	/**
	 * Check if status code is in the map
	 *
	 * @return boolean This returns true/false by checking if status code is in the map
	 */
	public static boolean containsKey(int statusCode) {
		return STATUS_CODE_MAP.containsKey(statusCode);
	}

	private HttpStatus() {
	}
}
