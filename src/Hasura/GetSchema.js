import "isomorphic-fetch";
import { getIntrospectionQuery, printSchema, buildClientSchema } from "graphql";

export function getGqlSchemaImpl({ url, headers }) {
  return async () => {
    try {
      const introspectionQuery = getIntrospectionQuery();

      const defaultHeaders = {
        "Content-Type": "application/json",
      };

      const response = await fetch(url, {
        method: "POST",
        headers: { ...defaultHeaders, ...headers },
        body: JSON.stringify({ query: introspectionQuery }),
        credentials: "include",
      });

      const { data, errors } = await response.json();

      if (errors) {
        throw errors;
      }

      const schema = printSchema(buildClientSchema(data));

      return schema;
    } catch (err) {
      console.error("failed to get gql schema", err);
      throw err;
    }
  };
}
